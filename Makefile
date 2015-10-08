all:
	rebar get-deps co generate
	redis-server &
	./rel/wechat_mud/bin/wechat_mud console