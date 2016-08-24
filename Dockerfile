FROM shuieryin/erlang:19.0

MAINTAINER Shuieryin <shuieryin@gmail.com>

ENV container docker

LABEL "version"="0.2.0" \
	"description"="Wechat mud game." \
	"author"="Shuieryin"

WORKDIR /root/workspaces
#Install & Run wechat_mud
RUN git clone https://github.com/shuieryin/wechat_mud.git ;\
	cd wechat_mud ;\
	git config --global user.email "shuieryin@gmail.com" ;\
	git config --global user.name "shuieryin" ;\
	make

CMD ["/usr/sbin/init"]
#Create container with ssh:
#	docker run --privileged -ti -v /sys/fs/cgroup:/sys/fs/cgroup:ro --name=wechat_mud --hostname=wechat_mud -p 13579:13579 -p 14369:4369 -p 32:22 shuieryin/wechat_mud:0.1.2
#Change root password:
#	docker exec -ti wechat_mud passwd

#Create container without ssh:
#	docker run -ti --link starbound:starbound.local --name=wechat_mud --hostname=wechat_mud.local -p 13579:13579 -p 4369:4369 -v /home/shuieryin/workspaces/wechat_mud:/root/workspaces/wechat_mud shuieryin/wechat_mud:latest /bin/bash