# Synopsis
Paraslonik is a small web crawler accompanied with a query execution engine. It runs across the html pages in a specified domain indexing the content. After the index is built it can be used to answer subsequent queries

# Usage
  Paraslonik accepts command line arguments having the form:
  `Paraslonik <site-url> <subcommand>`

## Building the index

First you need to build the index. It's done using `paraslonik reindex` subcommand.

It accepts a number of options:
 + -w or --num-workers WORKERS - a number of worker threads crawling the site in parallel
 + -A or --agent-string AGENT - an agent string sent in http requests
 + -d or --depth DEPTH - an integer restricting the link depth to be crawled
 + -p or --max-pages PAGES - restricts the maximum number of pages to ge crawled
 + --domain DOMAIN - By default Paraslonik doesn't leave the origin domain, this option can be used to make it crawle subdomains, just specify a common domain suffix
 + --query REGEX - An optional test to crawle only urls with query matching the Posix regular expression
 + --ignore-query REGEX - An optional test to ignore urls matching the reguler expression

So `Paraslonik http://example.com reindex --ignore-query '.+'` crawles all the pages of http://example.com with empty query part (this way you avoid lengthy wanders in blog calendars, for example)

## Running the queries

Hopefully, after a while an index got built. Downloaded pages are cached on disk, so you can interrupt the process and start it again without an additional server load. Now you can search the pages using the command `Paraslonik <site-url> search <query>`.

Query is one or several words separated by spaces. Pages containing all the words are returned, if they exist. If not, for every word in a query several pages are returned in the order of decreasing word frequency.

# Implementation details

Code is very simplistic for now. There is a single work queue and several threads pulling the tasks out of it. Every thread runs a loop which downloads a page, parses it and enqueues links for further analysis. The resulting stream of tag is used to build a page word frequency table. These tables are aggregated in a global index mapping words to urls and frequencies.

The index is serialized and stored to disk after crawling is finished. Then executing a query, it's loaded to memory and used to find relevant pages. Although simple, this approach severely limits the size of a site it's possible to analyze. Run with caution on sites consisting of more than sereval tens of thousands of pages.
