(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Standard footer for all pages *)

open Htmlit

let footer =
  El.splice [
    El.p [
      El.em [
        El.txt "This site is © 1998-2025 Anil Madhavapeddy, all rights reserved, except where the content is otherwise licensed. There are no third-party trackers. You can follow me on the usual social media and some self-hosted ones."
      ]
    ];
    El.p [
      El.em [
        El.txt "Chat (";
        El.a ~at:[At.rel "me"; At.class' "noicon"; At.href "https://bsky.app/profile/anil.recoil.org"] [El.txt "Bluesky"];
        El.txt " / ";
        El.a ~at:[At.rel "me"; At.class' "noicon"; At.href "https://amok.recoil.org/@avsm"] [El.txt "Mastodon"];
        El.txt " / ";
        El.a ~at:[At.class' "noicon"; At.href "https://www.linkedin.com/in/anilmadhavapeddy/"] [El.txt "LinkedIn"];
        El.txt " / ";
        El.s [El.txt "Twitter"];
        El.txt ")";
        El.br ();
        El.txt "Video (";
        El.a ~at:[At.class' "noicon"; At.href "https://crank.recoil.org/@avsm"] [El.txt "Recoil"];
        El.txt " / ";
        El.a ~at:[At.class' "noicon"; At.href "https://watch.eeg.cl.cam.ac.uk"] [El.txt "EEG"];
        El.txt " / ";
        El.a ~at:[At.class' "noicon"; At.href "https://watch.ocaml.org"] [El.txt "OCaml"];
        El.txt ")";
        El.br ();
        El.txt "Code (";
        El.a ~at:[At.class' "noicon"; At.href "https://github.com/avsm"] [El.txt "GitHub"];
        El.txt " / ";
        El.a ~at:[At.class' "noicon"; At.href "https://gitlab.developers.cam.ac.uk/avsm2"] [El.txt "GitLab@cam"];
        El.txt " / ";
        El.a ~at:[At.class' "noicon"; At.href "https://tangled.org/@anil.recoil.org"] [El.txt "Tangled"];
        El.txt ")";
        El.br ();
        El.txt "Feed (";
        El.a ~at:[At.href "/news.xml"] [
          El.txt "Atom ";
          El.img ~at:[At.class' "inline-icon"; At.alt "atom"; At.src "/assets/rss.svg"] ()
        ];
        El.txt " / ";
        El.a ~at:[At.href "/perma.xml"] [
          El.txt "Perma ";
          El.img ~at:[At.class' "inline-icon"; At.alt "atom"; At.src "/assets/rss.svg"] ()
        ];
        El.txt " / ";
        El.a ~at:[At.href "/feed.json"] [
          El.txt "JSON ";
          El.img ~at:[At.class' "inline-icon"; At.alt "json"; At.src "/assets/rss.svg"] ()
        ];
        El.txt " / ";
        El.a ~at:[At.href "/perma.json"] [
          El.txt "Perma JSON ";
          El.img ~at:[At.class' "inline-icon"; At.alt "json"; At.src "/assets/rss.svg"] ()
        ];
        El.txt ")"
      ]
    ]
  ]
