all:
	rebar get-deps co; \
	cd rel; \
	rm -rf wechat_mud wechat_mud_latest; \
	rebar generate; \
	mv wechat_mud wechat_mud_latest

upgrade:
	rebar co; \
	cd rel; \
	rm -rf wechat_mud; \
	sed -i.bak 's/$(OLD_VER)/$(NEW_VER)/1' reltool.config; \
	rebar generate; \
	rebar generate-appups previous_release=wechat_mud_latest; \
	rebar generate-upgrade previous_release=wechat_mud_latest; \
	mv wechat_mud_$(NEW_VER).tar.gz wechat_mud_latest/releases/; \
	rm -rf wechat_mud
