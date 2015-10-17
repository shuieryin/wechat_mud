all:
	rebar3 compile
	rebar3 release

upgrade:
	rebar3 compile
	#sed -i.bak 's/$(OLD_VER)/$(NEW_VER)/1' reltool.config
	#rebar3 relup
	#rebar3 install $(NEW_VER)