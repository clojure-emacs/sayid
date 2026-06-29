# Releasing Sayid

Sayid ships two artifacts: the Clojure library/middleware (published to Clojars)
and the `sayid` Emacs package (published to MELPA from the same repo). The version
string is hardcoded in a few places rather than derived at build time, so cutting
a release starts with bumping all of them.

## 1. Bump the version

Set the new version in:

- `project.clj` - the `defproject` coordinate
- `src/com/billpiel/sayid/core.clj` - `version`
- `src/sayid/plugin.clj` - `version`
- `src/el/sayid.el` - the `Version:` header, `sayid-version`, and `sayid-injected-plugin-version`
- `README.md` and `doc/nrepl-api.md` - the dependency snippets

## 2. Update the changelog

Move the `## unreleased` heading to `## [x.y.z] - YYYY-MM-DD`.

## 3. Sanity check

```sh
make test-all   # Clojure suite across the version matrix
make kondo      # Clojure lint
make elisp      # byte-compile + checkdoc the Emacs client
```

## 4. Deploy to Clojars

Sayid is published primarily under `mx.cider/sayid`. The legacy
`com.billpiel/sayid` coordinates are kept alive for now so existing dependencies
don't break, so each release is deployed twice.

You'll need `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` in the environment (a
Clojars deploy token), and deploy rights on both groups.

```sh
# Primary coordinates (mx.cider/sayid), as written in project.clj:
lein deploy clojars

# Legacy coordinates (com.billpiel/sayid): deploy the same build under the old
# group, then revert. The version is hardcoded, so the jar is identical bar the
# pom's groupId.
sed -i '' 's|defproject mx.cider/sayid|defproject com.billpiel/sayid|' project.clj
lein deploy clojars
git checkout project.clj
```

Once the legacy group is no longer worth maintaining, drop the second deploy and
the note in the README.

## 5. Tag and push

```sh
git tag -a vX.Y.Z -m "X.Y.Z"
git push origin master --tags
```

MELPA picks up the Emacs package from the repo automatically.
