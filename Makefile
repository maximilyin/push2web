PROJECT = push2web

DEPS = lager cowboy jsx uuid octopus wolfmq epgsql

dep_lager 	= git https://github.com/basho/lager.git            3.1.0
dep_cowboy  = git https://github.com/ninenines/cowboy.git       1.0.4
dep_jsx     = git https://github.com/talentdeficit/jsx.git      v2.7.1
dep_uuid    = git https://github.com/avtobiff/erlang-uuid.git   v0.5.0
dep_octopus = git https://github.com/erlangbureau/octopus.git   1.0.2
dep_wolfmq  = git https://github.com/erlangbureau/wolfmq.git    0.2.1
dep_epgsql  = git https://github.com/maximilyin/epgsql.git      3.1.2

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
