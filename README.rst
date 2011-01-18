About
-----

couchapp_legacy is a a new CouchApp engine for CouchDB.

Install
-------

Make sure Couchdb is installed on folder above the couchapp_engine one. You
can also change the path of couchdb installation by passing the path to
**COUCHDB_SRC* environment variable..

::

    $ make

(note during development you can use *make dev* command).


Add couch_zmq_pubsub server to couch configuration file *local_dev.ini*
(or in production local.ini) and edit the following section::

    [daemons]
    couchapp_legacy_routes={couchapp_legacy_routes, start_link, []}

    [httpd_design_handlers]
    _app = {couchapp_legacy_httpd, handle_app_req}

    [couchapp_legacy_handlers]
    rewrite = {couchapp_legacy_handlers, rewrite_handler}
    proxy = {couchapp_legacy_handlers, proxy_handler}


Start couchdb::

    $ export $COUCHAPP_LEGACY=/path/to/couchapp_legacy
    $ ERL_FLAGS="-pa $COUCHAPP_LEGACY/ebin -pa $COUCHAPP_LEGACY/deps/ibrowse/ebin" ./utils/run
    

Now you can test the couchapp_legacy rewriter. First install `couchapp
<http://github.com/couchapp/couchapp>`_ then go in the
examples/legacyapp folder and do::

    $ couchapp push testdb

This app for now allows you to test the rewriter. Rewrites rules are put
in the routes property of your design documen::

    [
        {
            "from": "^/blog/(?<post_no>\\w*)$",
            "to": "/_show/post/(?<post_no>)"
        },
        {    
            "from": "^/about$", 
            "to": "about.html",
            "type": "attachments"
        },
        {
            "from": "/page/:page",
            "to": "/_show/post/:page",
            "options": {
                "patterns": {
                    "page": "\\w*"
                }
            }
        },
        {
            "from": "/(?<func>\\w*)-(?<name>\\w*)/(?<post_no>\\w*)$",
            "to": "/_(?<func>)/(?<name>)/(?<post_no>)"
            
        },
        {
            "from": "^/$",
            "to": "index.html"
        },
        {
            "from": "^/blah",
            "to": "index.html"
        },
        {
            "from": "^/index.html$"
        }
    ]

The couchapp_legacy rewriter allows you to pass any regexp to your rule
or use the reversed dispatching currently use in default CouchDB
rewriter.

For example go on http://127.0.0.1:5984/testdb/_design/test/_app/
url. You will see the welcome page. This correspond to the rule::

    {
        "from": "^/$",
        "to": "index.html"
    }

You can also do such rule::

    {
            "from": "^/blog/(?<post_no>\\w*)$",
            "to": "/_show/post/(?<post_no>)"
    }

Go on http://127.0.0.1:5984/testdb/_design/test/_app/blog/test for
example to see the result. 

Or more complex rule:

    {
        "from": "/(?<func>\\w*)-(?<name>\\w*)/(?<post_no>\\w*)$",
        "to": "/_(?<func>)/(?<name>)/(?<post_no>)"
    }

Url http://127.0.0.1:5984/testdb/_design/test/_app/show-post/test 

is rewritten to 

http://127.0.0.1:5984/testdb/_design/test/_show/post/test

Reverse url dispatching is working too::

    {
        "from": "/page/:page",
        "to": "/_show/post/:page",
        "options": {
            "patterns": {
                "page": "\\w*"
            }
        }
    }

Url http://127.0.0.1:5984/testdb/_design/test/_app/page/test 

is rewritten to 

http://127.0.0.1:5984/testdb/_design/test/_app/_show/post/test

Note that you need to fix patterns here to have reverse dispatching
working, which is a litte different from default couchapp engine.


More doc soon.

TODO:
-----

- Add Query paramenter in subtitutions variables 
- Replace shows/updates/lists by improved code.


Changelog:
---------

version 0.01:
+++++++++++++

 - New Couchapp Rewriter
