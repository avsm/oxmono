"""Owner isolation and deterministic source releases, without network or compilers."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import shutil
import sys
import tarfile
import tempfile

OX = str(Path(sys.argv[1]).resolve())

def write(p, text):
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(text)

def git(root, *args):
    return subprocess.run(['git', '-C', str(root), *args], check=True,
                          capture_output=True, text=True).stdout.strip()

release_tags = {}

def call(root, *args, ok=True):
    if len(args)>1 and args[0]=='release' and args[1] in ['plan','export'] and '--plan' not in args:
        args=list(args)
        commit=git(root,'rev-parse','HEAD')
        if '--tag' in args:
            tag=args[args.index('--tag')+1]
        else:
            if commit not in release_tags:
                tags=git(root,'tag','--list','20261101-*').splitlines()
                epoch=max([int(t.split('-')[1]) for t in tags]+[0])+1
                release_tags[commit]=f'20261101-{epoch}'
            tag=release_tags[commit]
            args+=['--tag',tag]
        if not git(root,'tag','--list',tag):git(root,'tag',tag,'HEAD')
    p = subprocess.run([OX, *map(str, args)], cwd=root, text=True,
                       capture_output=True, timeout=90)
    assert (p.returncode == 0) == ok, (args, p.stdout, p.stderr)
    return p

with tempfile.TemporaryDirectory(prefix='ox-management-') as tmp:
    tmp = Path(tmp)
    root = tmp / 'repo'
    root.mkdir()
    git(root, 'init', '-q')
    write(root / 'dune-project', '(lang dune 3.21)\n(name fixture)\n')
    write(root / 'dune', '(vendored_dirs vendor)\n')
    write(root / 'u/alice/user.json', json.dumps({'owner':'alice','include':[]}))
    def pkg(path, name, deps=''):
        write(root / path / 'dune-project', f'(lang dune 3.21)\n(name {name})\n(version 1.0)\n(generate_opam_files true)\n')
        write(root / path / (name+'.opam'), f'opam-version: "2.0"\ndepends: [{deps}]\n')
        write(root / path / 'value', 'original\n')
    pkg('u/alice/app', 'app', '"shared"')
    pkg('wip/shared', 'shared')
    write(root/'wip/shared/dune', '(library (name shared) (public_name shared))\n')
    write(root/'wip/shared/shared.ml', 'let value = "owner bundle works"\n')
    write(root/'u/alice/app/dune', '(executable (name main) (public_name app) (libraries shared))\n(install (section share) (files asset.txt))\n')
    write(root/'u/alice/app/main.ml', 'let () = print_endline Shared.value\n')
    write(root/'u/alice/app/asset.txt','installed asset\n')
    # An invalid unrelated project must not be parsed for Alice.
    pkg('u/bob/broken', 'app')
    write(root / 'u/bob/broken/app.opam', 'not opam at all')
    inventory=json.loads(call(root, 'inventory', '--user', 'alice', '--check').stdout)
    assert {p['name'] for p in inventory['packages']} == {'app','shared'}
    stage=tmp/'stage'
    call(root,'inventory','--user','alice','--stage',stage)
    assert (stage/'u/alice/app/value').exists()
    assert not (stage/'u/bob').exists()
    assert not (stage/'.git').exists()
    bundle=tmp/'bundle'
    call(root,'dist','bundle','--prepared','--user','alice','--output',bundle)
    assert subprocess.check_output([str(bundle/'bin/app')],text=True).strip()=='owner bundle works'
    assert (bundle/'share/app/asset.txt').read_text()=='installed asset\n'
    moved=tmp/'moved-bundle'
    bundle.rename(moved)
    assert subprocess.check_output([str(moved/'bin/app')],text=True).strip()=='owner bundle works' 
    # No implicit cross-owner dependency through the repository solver.
    write(root/'u/alice/app/app.opam', 'opam-version: "2.0"\ndepends: ["private"]\n')
    pkg('u/bob/private','private')
    assert 'excluded owner' in call(root,'inventory','--user','alice',ok=False).stderr
    write(root/'u/alice/app/app.opam', 'opam-version: "2.0"\ndepends: ["shared"]\n')
    pkg('wip/unrelated','unrelated')
    write(root/'u/alice/user.json',json.dumps({'owner':'alice','include':['unrelated']}))
    git(root,'add','.')
    git(root,'-c','user.name=Fixture','-c','user.email=fixture@example.invalid','commit','-qm','fixture')
    write(root/'wip/shared/value','uncommitted must not escape\n')
    # Dune quoted-line strings are not ordinary S-expressions.
    dune_project=root/'wip/shared/dune-project'
    dune_project.write_text(dune_project.read_text()+'(package (name shared) (description\n "\\> A description with (parentheses) and an unclosed quote: \"\n))\n')
    write(root/'wip/shared/shared.opam','opam-version: "2.0"\nversion: "1.0"\n')
    git(root,'add','wip/shared/dune-project','wip/shared/shared.opam')
    git(root,'-c','user.name=Fixture','-c','user.email=fixture@example.invalid','commit','-qm','Dune string')
    # Use an explicit fake upstream guard, preserving its build/post filters.
    guards=tmp/'guards'
    write(guards/'repo','opam-version: "2.0"\n')
    write(guards/'packages/oxcaml-shared-patches/oxcaml-shared-patches.enabled/opam',
          'opam-version: "2.0"\ndepends: ["shared" {build & = "0.1+ox"} "oxcaml" {post}]\nconflicts: ["oxcaml-shared"]\n')
    def export(out):
        call(root,'release','export','--user','alice','--base-url','https://example.invalid/releases/one',
             '--guard-repository',guards,'--output',out)
    call(root,'release','plan','--user','alice','--base-url','https://example.invalid','--output',tmp/'missing-guards',ok=False)
    call(root,'release','plan','--user','alice','--base-url','http://example.invalid',
         '--guard-repository',guards,'--output',tmp/'bad-url',ok=False)
    for invalid in ['20260230-1','20261101-0','20261101-01','20261301-1']:
        assert 'valid date' in call(root,'release','plan','--user','alice','--tag',invalid,
             '--base-url','https://example.invalid','--guard-repository',guards,
             '--output',tmp/invalid,ok=False).stderr
    a,b=tmp/'one',tmp/'two'
    export(a)
    plan=tmp/'plan.json'
    call(root,'release','plan','--user','alice','--base-url','https://example.invalid/releases/one',
         '--guard-repository',guards,'--output',plan)
    planned=json.loads(plan.read_text())
    tag=planned['tag'];commit=planned['commit']
    other=git(root,'-c','user.name=Fixture','-c','user.email=fixture@example.invalid',
              'commit-tree',git(root,'rev-parse','HEAD^{tree}'),'-p',commit,'-m','another commit')
    git(root,'tag','-f',tag,other)
    assert 'does not match' in call(root,'release','export','--plan',plan,'--output',tmp/'moved',ok=False).stderr
    git(root,'tag','-f',tag,commit)
    call(root,'release','export','--plan',plan,'--output',b)
    def contents(p):
        return {str(f.relative_to(p)):hashlib.sha256(f.read_bytes()).hexdigest()
                for f in p.rglob('*') if f.is_file()}
    assert contents(a)==contents(b)
    call(root,'release','verify',a)
    manifest=json.loads((a/'manifest.json').read_text())
    for unit in manifest['archives']:
        with tarfile.open(a/unit['archive']) as tar:
            assert tar.extractfile('value').read()==b'original\n'
            project=tar.extractfile('dune-project').read().decode()
            assert 'generate_opam_files false' in project
            if unit['project']=='wip/shared': assert 'A description with (parentheses)' in project
    opam=(a/'opam-repository/packages/app/app.1.0+ox-20261101-1/opam').read_text()
    assert 'oxmono-snapshot' not in opam
    assert not (a/'opam-repository/packages/oxmono-snapshot').exists()
    assert 'x-subpath' not in opam
    assert '5.2.0minus39' in opam and 'shared' in opam and '1.0+ox' in opam
    marker=(a/'opam-repository/packages/oxcaml-shared-patches/oxcaml-shared-patches.enabled/opam').read_text()
    assert '1.0+ox' in marker and '0.1+ox' in marker and 'build' in marker and 'post' in marker
    # Only actual dependencies constrain installation, not unrelated packages.
    if shutil.which('opam'):
        solver_repo=tmp/'solver-repo'
        shutil.copytree(a/'opam-repository',solver_repo)
        def recipe(name,version,body=''):
            write(solver_repo/'packages'/name/(name+'.'+version)/'opam',
                  'opam-version: "2.0"\n'+body+'\n')
        recipe('oxcaml-compiler','5.2.0minus39', 'depends: [("oxcaml-shared" {post} | "oxcaml-shared-patches" {post})]')
        recipe('oxcaml-shared','guard', 'conflicts: ["shared" "oxcaml-shared-patches"]')
        recipe('oxcaml-compiler','5.2.0minus40')
        recipe('oxcaml','latest')
        recipe('shared','9.0')
        recipe('unrelated','9.0')
        env=dict(os.environ,OPAMROOT=str(tmp/'opam-root'),OPAMYES='1',OPAMCONFIRMLEVEL='unsafe-yes')
        def opam(*args,ok=True):
            result=subprocess.run(['opam',*map(str,args)],env=env,cwd=tmp,
                                  capture_output=True,text=True,timeout=90)
            assert (result.returncode==0)==ok,(args,result.stdout,result.stderr)
        opam('init','--bare','--no-setup','--disable-sandboxing','fixture',solver_repo)
        opam('switch','create','fixture','--empty')
        opam('install','--dry-run','app.1.0+ox-20261101-1')
        opam('install','--dry-run','app.1.0+ox-20261101-1','unrelated.9.0')
        opam('install','--dry-run','app.1.0+ox-20261101-1','oxcaml-compiler.5.2.0minus40',ok=False)
        opam('install','--dry-run','app.1.0+ox-20261101-1','shared.9.0',ok=False)
    # A new tag or source URL alone must not manufacture new packages.
    noop_plan=tmp/'noop-plan.json'
    call(root,'release','plan','--user','alice','--tag','20261101-2',
         '--base-url','https://example.invalid/next','--guard-repository',guards,
         '--previous',a,'--output',noop_plan)
    assert json.loads(noop_plan.read_text())['no_changes'] is True
    assert 'No package changes' in call(root,'release','export','--plan',noop_plan,'--output',tmp/'noop').stderr
    assert not (tmp/'noop').exists()
    original_manifest=(a/'manifest.json').read_bytes()
    changed_manifest=json.loads(original_manifest)
    changed_manifest['note']='changed after planning'
    (a/'manifest.json').write_text(json.dumps(changed_manifest))
    assert 'manifest changed since planning' in call(root,'release','export','--plan',noop_plan,
        '--output',tmp/'changed-previous',ok=False).stderr
    (a/'manifest.json').write_bytes(original_manifest)

    original_commit=git(root,'rev-parse','HEAD')
    write(root/'notes','unrelated monorepo change\n')
    git(root,'add','notes')
    git(root,'-c','user.name=Fixture','-c','user.email=fixture@example.invalid','commit','-qm','unrelated')
    call(root,'release','export','--user','alice','--base-url','https://example.invalid/next',
         '--guard-repository',guards,'--previous',a,'--output',tmp/'unrelated-noop')
    assert not (tmp/'unrelated-noop').exists()
    write(root/'u/alice/app/value','application changed\n')
    git(root,'add','u/alice/app/value')
    git(root,'-c','user.name=Fixture','-c','user.email=fixture@example.invalid','commit','-qm','application')
    newer=tmp/'newer'
    call(root,'release','export','--user','alice','--base-url','https://example.invalid/releases/two',
         '--guard-repository',guards,'--previous',a,'--output',newer)
    def records(directory):
        return {r['project']:r for r in json.loads((directory/'manifest.json').read_text())['archives']}
    old_records,new_records=records(a),records(newer)
    for name in ['wip/shared','wip/unrelated']:
        assert new_records[name]['reused']
        assert new_records[name]['checksum']==old_records[name]['checksum']
        assert new_records[name]['packages']==old_records[name]['packages']
    assert not new_records['u/alice/app']['reused']
    assert (newer/'opam-repository/packages/shared/shared.1.0+ox-20261101-1/opam').read_bytes()==(a/'opam-repository/packages/shared/shared.1.0+ox-20261101-1/opam').read_bytes()
    call(root,'release','verify',newer)
    # Updating a dependency changes its dependents' embedded exact constraints.
    write(root/'wip/shared/value','dependency changed\n')
    git(root,'add','wip/shared/value')
    git(root,'-c','user.name=Fixture','-c','user.email=fixture@example.invalid','commit','-qm','dependency')
    propagated=tmp/'propagated'
    call(root,'release','export','--user','alice','--base-url','https://example.invalid/releases/three',
         '--guard-repository',guards,'--previous',newer,'--output',propagated)
    active=records(propagated)
    assert not active['wip/shared']['reused'] and not active['u/alice/app']['reused']
    assert active['wip/unrelated']['reused']
    call(root,'release','verify',propagated)
    if shutil.which('opam'):
        shutil.copytree(propagated/'opam-repository',solver_repo,dirs_exist_ok=True)
        opam('update','fixture')
        new_app=active['u/alice/app']['packages'][0]
        new_shared=active['wip/shared']['packages'][0]
        old_unrelated=active['wip/unrelated']['packages'][0]
        opam('install','--dry-run',new_app,old_unrelated)
        opam('install','--dry-run',new_app,'shared.1.0+ox-20261101-1',ok=False)
        opam('install','--dry-run','app.1.0+ox-20261101-1',new_shared,ok=False)
        opam('install','--dry-run',new_app,'shared.9.0',ok=False)
    # Even a forcibly moved tag cannot overwrite an existing source version.
    git(root,'tag','-f','20261101-1','HEAD')
    call(root,'release','export','--user','alice','--tag','20261101-1',
         '--base-url','https://example.invalid/collision','--guard-repository',guards,
         '--previous',a,'--output',tmp/'collision',ok=False)
    git(root,'tag','-f','20261101-1',original_commit)
    assert not (tmp/'collision').exists()
    with (a/manifest['archives'][0]['archive']).open('ab') as f:f.write(b'bad')
    assert 'Checksum mismatch' in call(root,'release','verify',a,ok=False).stderr
    assert 'Checksum mismatch' in call(root,'release','export','--user','alice',
        '--base-url','https://example.invalid/next','--guard-repository',guards,
        '--previous',a,'--tag','20261101-3','--output',tmp/'corrupt-previous',ok=False).stderr
    # New owners get a concrete executable and substituted container recipe.
    write(root/'tools/user-template/Dockerfile', 'owner @OWNER@\n')
    write(root/'tools/user-template/Dockerfile.dockerignore', '!u/@OWNER@/**\n')
    call(root,'init','--user','charlie')
    assert (root/'u/charlie/Dockerfile').read_text()=='owner charlie\n'
    assert (root/'u/charlie/hello/hello-charlie.opam').exists()
    call(root,'init','--user','charlie',ok=False)
    # Flattened project names must not share an extraction directory.
    pkg('wip/a-b','flat')
    pkg('wip/a/b','nested')
    write(root/'wip/a-b/only-flat','must not leak\n')
    git(root,'add','.')
    git(root,'-c','user.name=Fixture','-c','user.email=fixture@example.invalid','commit','-qm','paths')
    paths=tmp/'paths'
    call(root,'release','export','flat','nested','--base-url','https://example.invalid/paths',
         '--guard-repository',guards,'--output',paths)
    for unit in json.loads((paths/'manifest.json').read_text())['archives']:
        with tarfile.open(paths/unit['archive']) as tar:
            assert ('only-flat' in tar.getnames()) == (unit['project']=='wip/a-b')
    # Release bases can be declared centrally without modifying vendored sources.
    write(root/'release-versions.json',json.dumps({'schema_version':1,'projects':{
        'wip/a-b':{'base_version':'2.1.0','reason':'Initial port release'}}}))
    git(root,'add','release-versions.json')
    git(root,'-c','user.name=Fixture','-c','user.email=fixture@example.invalid','commit','-qm','release base')
    overrides=tmp/'overrides'
    call(root,'release','export','flat','--base-url','https://example.invalid/overrides',
         '--guard-repository',guards,'--previous',paths,'--output',overrides)
    assert list((overrides/'opam-repository/packages/flat').glob('flat.2.1.0+ox-20261101-*/opam'))
    # Reject source links which could pull data from outside a release unit.
    (root/'wip/shared/escape').symlink_to('/etc/passwd')
    git(root,'add','wip/shared/escape')
    git(root,'-c','user.name=Fixture','-c','user.email=fixture@example.invalid','commit','-qm','bad link')
    call(root,'release','export','--user','alice','--base-url','https://example.invalid',
         '--guard-repository',guards,'--output',tmp/'bad',ok=False)
    assert not (tmp/'bad').exists()
    (root/'wip/shared/escape').unlink()
    victim=tmp/'untouched-project'
    victim.write_text('(lang dune 3.21)\n(name shared)\n(version 1.0)\n')
    (root/'wip/shared/dune-project').unlink()
    (root/'wip/shared/dune-project').symlink_to(victim)
    # A version in the opam file lets discovery reach the extraction check.
    write(root/'wip/shared/shared.opam','opam-version: "2.0"\nversion: "1.0"\n')
    git(root,'add','.')
    git(root,'-c','user.name=Fixture','-c','user.email=fixture@example.invalid','commit','-qm','metadata link')
    before=victim.read_bytes()
    call(root,'release','export','--user','alice','--base-url','https://example.invalid',
         '--guard-repository',guards,'--output',tmp/'metadata-link',ok=False)
    assert victim.read_bytes()==before

    git(root,'config','user.name','Fixture')
    git(root,'config','user.email','fixture@example.invalid')
    call(root,'release','tag','--tag','20261102-1')
    assert git(root,'cat-file','-t','refs/tags/20261102-1')=='tag'
    call(root,'release','tag','--tag','20261102-1',ok=False)
    write(root/'wip/a-b/only-flat','dirty change\n')
    assert 'Commit working-tree changes' in call(root,'release','tag','--tag','20261102-2',ok=False).stderr
print('ox management: isolated owners, committed deterministic exports, guards and verification: OK')
