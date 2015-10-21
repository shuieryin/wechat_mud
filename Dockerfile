FROM shuieryin/erlang:latest

MAINTAINER Collin Guo <shuieryin@gmail.com>

ENV container docker
ENV WORKSPACE_PATH=/root/workspaces

LABEL "version"="0.1.0" \
	"description"="Wechat mud game." \
	"author"="Collin Guo"

WORKDIR $WORKSPACE_PATH
#Install & Run wechat_mud
RUN  git clone https://github.com/shuieryin/wechat_mud.git \
	&& cd wechat_mud \
	&& git config --global user.email "shuieryin@gmail.com" \
	&& git config --global user.name "shuieryin" \
	&& make

CMD ["/usr/sbin/init"]
#Create container:
#	docker run --privileged -ti -v /sys/fs/cgroup:/sys/fs/cgroup:ro --name=wechat_mud -p 13579:13579 -p 32:22 shuieryin/wechat_mud
#Change root password:
#	docker exec -ti wechat_mud passwd
