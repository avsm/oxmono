"""Create the deterministic local Zulip fixture for the OCaml integration suite.

This script is executed through the pinned server image's ``manage.py shell``.
It intentionally uses Zulip's realm, user and channel actions rather than SQL,
so the test data follows the invariants of the version under test.
"""

from __future__ import annotations

import json
import os
from pathlib import Path

from zerver.actions.create_realm import do_create_realm
from zerver.actions.create_user import do_create_user
from zerver.actions.streams import bulk_add_subscriptions
from zerver.lib.streams import create_stream_if_needed
from zerver.models import Realm, UserProfile


# The root realm makes the server's canonical host and the loopback URL agree.
# A subdomain realm would require each test process to provide a custom DNS
# mapping for `zulip-test.localhost.localdomain`.
REALM_ID = ""
PASSWORD = "fixture-password-not-for-network-use"


def user(email: str, full_name: str, realm: Realm, **kwargs: object) -> UserProfile:
    existing = UserProfile.objects.filter(realm=realm, delivery_email__iexact=email).first()
    if existing is not None:
        return existing
    return do_create_user(
        email,
        PASSWORD,
        realm,
        full_name,
        tos_version=UserProfile.TOS_VERSION_BEFORE_FIRST_LOGIN,
        acting_user=kwargs.pop("acting_user", None),
        **kwargs,
    )


def principal(profile: UserProfile) -> dict[str, object]:
    return {
        "id": profile.id,
        # The delivery address is the API Basic-auth identity.  `profile.email`
        # can be a privacy-preserving display address for this realm.
        "email": profile.delivery_email,
        "delivery_email": profile.delivery_email,
        "api_key": profile.api_key,
        "full_name": profile.full_name,
    }


realm = Realm.objects.filter(string_id=REALM_ID).first()
if realm is None:
    realm = do_create_realm(string_id=REALM_ID, name="OCaml Zulip integration")

admin = user(
    "admin@zulip.test",
    "Fixture Admin",
    realm,
    role=UserProfile.ROLE_REALM_OWNER,
    realm_creation=True,
)
alice = user("alice@zulip.test", "Alice Fixture", realm, acting_user=admin)
bob = user("bob@zulip.test", "Bob Fixture", realm, acting_user=admin)
echo = user(
    "echo-bot@zulip.test",
    "Echo Fixture Bot",
    realm,
    bot_type=UserProfile.DEFAULT_BOT,
    bot_owner=admin,
    acting_user=admin,
)
store = user(
    "store-bot@zulip.test",
    "Store Fixture Bot",
    realm,
    bot_type=UserProfile.DEFAULT_BOT,
    bot_owner=admin,
    acting_user=admin,
)

public, _ = create_stream_if_needed(
    realm, "ocaml-public", stream_description="Public channel for integration tests", acting_user=admin
)
private, _ = create_stream_if_needed(
    realm,
    "ocaml-private",
    invite_only=True,
    stream_description="Private channel for integration tests",
    acting_user=admin,
)
bulk_add_subscriptions(realm, [public], [admin, alice, bob, echo, store], acting_user=admin)
bulk_add_subscriptions(realm, [private], [admin, alice, echo], acting_user=admin)

port = os.environ["ZULIP_TEST_PORT"]
fixture = {
    "schema": 1,
    "server": {
        "url": f"http://127.0.0.1:{port}",
        "api_url": f"http://127.0.0.1:{port}/api/v1",
        "version": "12.2",
    },
    "realm": {"id": realm.id, "string_id": realm.string_id},
    "users": {
        "admin": principal(admin),
        "alice": principal(alice),
        "bob": principal(bob),
        "bots": {"echo": principal(echo), "store": principal(store)},
    },
    "channels": {
        "public": {"id": public.id, "name": public.name},
        "private": {"id": private.id, "name": private.name},
    },
    # Topics and group DMs only exist after a message is sent.  These stable
    # names/participants let scenarios create their own isolated messages.
    "topics": {"smoke": "smoke", "bot": "bot"},
    "group_dm": {"user_ids": [alice.id, bob.id, echo.id]},
}

path = Path("/ocaml-zulip-output/fixtures.json")
temporary = path.with_suffix(".tmp")
temporary.write_text(json.dumps(fixture, sort_keys=True, indent=2) + "\n")
temporary.replace(path)
print(path)
