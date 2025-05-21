FROM fedora:34
RUN dnf -y install httpd procps less; systemctl enable httpd
STOPSIGNAL SIGRTMIN+3
CMD [ "/sbin/init" ]
