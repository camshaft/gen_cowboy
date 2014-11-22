PROJECT = gen_cowboy

# dependencies

DEPS = simple_env cowboy_estack

dep_simple_env = git https://github.com/camshaft/simple_env.git
dep_cowboy_estack = git https://github.com/camshaft/cowboy_estack.git

include erlang.mk
