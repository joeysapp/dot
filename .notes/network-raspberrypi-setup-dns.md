# Raspberry Pi
Notes about the usage of raspberrypi

# TODO w/ switch+nic
Switch to 10.0.0.0/8 :^)
* https://en.wikipedia.org/wiki/Private_network

# Custom "ip-addr", "PTRs", "reverse DNS lookup"
To have custom hosts in your `arp -a` and local networking, do the following:
1. Setup pi-hole, assign static LAN address and point router to use that as its DNS.
2. SSH into pi-hole
3. sudo nano /etc/hosts
2. Add entries of:
`<ip-address>      <desired-name>`
4. Write out file, close
5. $ pihole restartdns

* Source: https://github.com/pi-hole/pi-hole/issues/975#issuecomment-281027117)

# LAN Development
To allow connections between local hosts while running the pi.hole as a custom DNS server, do the following:
1. Add `<local-ip-addres> <custom-local-name>` to `/etc/hosts` or `/etc/dnsmasq.d/NN-foo.conf` or `/etc/pihole/custom.list`
2. `pihole restartdns`
3. Set your server to startup on the desired port and your `local-ip-address`
4. All hosts on your network can navigate to `http://<foo>:<port>`

## Further reading on subnets:
* 53, ip-addr, "PTRs", reverse DNS lookup
* https://www.ietf.org/rfc/rfc1878.txt
* https://www.aelius.com/njh/subnet_sheet.html
* https://superuser.com/questions/1722394/modify-a-routers-reply-to-macoss-reverse-dns-query-such-that-hostname-defaults
* https://www.cloudflare.com/learning/dns/dns-records/dns-ptr-record/

I think pihole uses dnsmasq, this is just how it does it behind the scenes:
* https://thekelleys.org.uk/dnsmasq/docs/dnsmasq-man.html
* https://blog.mdoff.net/posts/2019/how-add-custom-dns-entries-in-pi-hole/
* https://www.howtogeek.com/devops/how-to-run-your-own-dns-server-on-your-local-network/
* https://serverfault.com/questions/36562/adding-custom-dns-entries-for-name-resolution-in-the-local-network
