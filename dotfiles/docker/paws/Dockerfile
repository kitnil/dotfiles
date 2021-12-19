FROM ubuntu:20.04

RUN apt-get update \
 && apt-get install -y build-essential perl libfile-slurp-perl libhtml-parser-perl libjson-xs-perl libwww-perl libmime-tools-perl libyaml-perl libipc-run3-perl libio-capture-perl libipc-shareable-perl libhttp-async-perl libio-async-perl libio-async-ssl-perl libdatetime-perl cpanminus

RUN cpanm Net::Async::WebSocket::Client constant::override

RUN apt-get update \
 && apt-get install -y git
run git clone https://github.com/tomhrr/paws /usr/local/src/paws

RUN cd /usr/local/src/paws \
 && perl Makefile.PL \
 && make \
 && make install

RUN apt-get update \
 && apt-get install -y sendmail

RUN groupadd --gid 998 user
RUN useradd --uid 1000 --gid 998 --create-home user
RUN mkdir /home/user/.paws \
 && chown user: /home/user/.paws

RUN cpanm Net::HTTPS::NB

USER user

ENTRYPOINT [ "paws-receive" ]
