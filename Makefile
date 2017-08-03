PROJECT = bitcoin_reg
PROJECT_DESCRIPTION = Bitcoin Registration
PROJECT_VERSION = 0.1.0

# Whitespace to be used when creating files from templates.
SP = 2

DEPS = lager cowboy eleveldb mnesia_eleveldb uuid sync jsx ejwt tempo 
dep_mnesia_eleveldb = git https://github.com/klarna/mnesia_eleveldb.git master
dep_eleveldb_commit = 2.2
dep_cowboy_commit = 1.1.2

LOCAL_DEPS = mnesia crypto inets sasl


include erlang.mk
