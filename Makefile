all:
	rebar get-deps co; \
	cd rel; \
	rm -rf wechat_mud wechat_mud_latest; \
	rebar generate; \
	mv wechat_mud wechat_mud_latest

upgrade:
	rebar co; \
	cd rel; \
	rebar generate; \
	rebar generate-appups previous_release=wechat_mud_latest; \
	rebar generate-upgrade previous_release=wechat_mud_latest
