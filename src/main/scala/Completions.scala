import org.jline.reader.impl.completer.StringsCompleter;

object Completions {

  def completer() : StringsCompleter = {
    new StringsCompleter(
      "_cat",
      "_cat/aliases",
      "_cat/allocation",
      "_cat/count",
      "_cat/fielddata",
      "_cat/health",
      "_cat/help",
      "_cat/indices",
      "_cat/master",
      "_cat/nodeattrs",
      "_cat/nodes",
      "_cat/pending_tasks",
      "_cat/plugins",
      "_cat/recovery",
      "_cat/repositories",
      "_cat/segments",
      "_cat/shards",
      "_cat/snapshots",
      "_cat/tasks",
      "_cat/templates",
      "_cat/thread_pool",
      "_search",
      "_search/scroll",
      "_search_shards",
      "_search/template",
      "_cluster/allocation/explain",
      "_cluster/settings",
      "_cluster/health",
      "_cluster/pending_tasks",
      "_cluster/reroute",
      "_cluster/state",
      "_cluster/stats",
      "_count",
      "_scripts",
      "_field_stats",
      "_analyze",
      "_cache/clear",
      "_template",
      "_flush",
      "_flush/synced",
      "_forcemerge",
      "_alias",
      "_mapping",
      "_mapping/field",
      "_settings",
      "_upgrade",
      "_recovery",
      "_refresh",
      "_segments",
      "_shard_stores",
      "_stats",
      "_shard_stores",
      "_shard_stores",
      "_aliases",
      "_validate/query",
      "_ingest/pipeline",
      "_ingest/pipeline/simulate",
      "_mget",
      "_msearch",
      "_msearch/template",
      "_mtermvectors",
      "_nodes/hot_threads",
      "_cluster/nodes/hot_threads",
      "_nodes",
      "_nodes/stats",
      "_reindex",
      "_render/template",
      "_snapshot",
      "_snapshot/status",
      "_tasks",
      "GET", 
      "PUT",
      "POST",
      "DELETE", 
      "HEAD"
    )
  }

}
