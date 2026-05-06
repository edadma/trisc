package io.github.edadma.trisc

/** Platform-abstract interface for fetching git dependencies. The CLI wires
 *  in a JVM-specific implementation that shells out to `git`. JS/Native
 *  builds can leave this `None` and the resolver will reject git deps
 *  with the same legacy error path it used before chunk 5.
 *
 *  The fetcher hides cache layout, network IO, and ref-resolution from the
 *  resolver so the resolver's tree walk stays pure (FileOps + decoded
 *  manifests). Tests can substitute an in-process implementation that points
 *  at a temp `~/.sysl` to stay hermetic.
 */
trait GitFetcher:
  /** Ensure a checkout exists for `(url, ref)` and return both the resolved
   *  SHA and the on-disk directory. The fetcher is allowed to short-circuit
   *  the network when `knownSha` is provided AND the on-disk checkout for
   *  that sha already exists — that's how `sysl fetch` honors lock pins.
   *
   *  `knownSha = None` always re-resolves the ref (used by `sysl update`
   *  and by first-time fetches with no lock pin).
   *
   *  Returns Right((sha, dir)) on success, Left(msg) on any failure
   *  (network, missing ref, malformed url, git not installed, etc.). */
  def ensureCheckout(
      url: String,
      refKind: GitRefKind,
      refName: String,
      knownSha: Option[String],
      offline: Boolean = false,
  ): Either[String, (String, String)]

  /** Cache root for diagnostic output. Implementations point this at
   *  `~/.sysl/cache` (or whatever `SYSL_CACHE_DIR` overrides it to). */
  def cacheRoot: String

/** Wire-format tag for a git ref. Mirrors the inline-table fields a sysl.toml
 *  manifest may carry (`rev = "..."`, `tag = "..."`, `branch = "..."`) and
 *  also drives the URL fragment we encode in `sysl.lock` source strings. */
enum GitRefKind:
  case Rev, Tag, Branch

object GitRefKind:
  def label(k: GitRefKind): String = k match
    case Rev    => "rev"
    case Tag    => "tag"
    case Branch => "branch"

  def fromLabel(s: String): Option[GitRefKind] = s match
    case "rev"    => Some(Rev)
    case "tag"    => Some(Tag)
    case "branch" => Some(Branch)
    case _        => None

  def fromGitRef(r: GitRef): (GitRefKind, String) = r match
    case GitRef.Rev(s)    => (Rev, s)
    case GitRef.Tag(s)    => (Tag, s)
    case GitRef.Branch(s) => (Branch, s)

/** Single-instance hand-off slot, mirroring `FileOps.instance`. The JVM `Main`
 *  installs a `JvmGitFetcher` here before the CLI dispatches; JS/Native main
 *  entry points leave it `None` so git deps fail with the standard "not
 *  supported here" error rather than crashing on a missing impl. */
object GitFetcherProvider:
  var instance: Option[GitFetcher] = None
