# Pubkey Auth Setup Checklist
If you follow these steps, you can connect to a host and allow a client connection. Assumed you have keys already.
Kittenblob and Foxy are real names, they're two fish I own. When appropriate I refer to them as K and F.

# "The Pubkey exchange between host and client"
- [ ] 1. [Host, Kblob] Kittenblob sends us their host pubkey file                    (k:/etc/ssh/kittenblob_host_edd.pub) -> f:~/.ssh/
- [ ] 2. [Client,  us] We send Kittenblob our client pubkey file                     (f:~/.ssh/foxy_client_edd.pub)       -> k:/etc/ssh/
   	
# "The Client config"
- [ ] 3. [Client,  us] We "add" Kittenblob and their details to our config           (f:~/.ssh/config)
         * "Host" is 'kittenblob.local' (set on their side)
         * "HostName" is **NORMALLY** a local address, here, kittenblob.local
         * "User" is zooey
         * "IdentityFile" is our (Foxy) private client key file                       (f:~/.ssh/foxy_client_edd)
- [ ] 4. [Client,  us] We "add" Kittenblob's pubkey AND the "HostName"                (f:~/.ssh/known hosts)
         * to our known hosts file. This example is unique - is kittenblob.local.
   	
# "The Host's One-time setup"
If you have a trusted clients file already and have added it and your (host's)
private key in the sshd config file, you can skip to the final step.
- [ ] 5. [Host, Kblob] Kittenblob makes a "VeryCoolClients" file:
         * `$ touch /etc/ssh/VeryCoolClients;`
- [ ] 6. [Host, Kblob] Kittenblob "adds" that file to sshd config and their own privkey: 
         ```
             /etc/ssh/sshd_config
             ...
		     AuthorizedKeysFile      /etc/ssh/VeryCoolClients
		     HostKey                 /etc/ssh/foxy_host_edd
		     PubkeyAuthentication    yes
		     PasswordAuthentication  no
             ...
         ```
# "The Host config"
- [ ] 7. [Host, Kblob] Kittenblob adds our (Foxy's) public key to VeryCoolClients.      (k:/etc/ssh/KoolClients)
         * That pubkey was sent by us, the client, to Kittenblob the host in step #2.
   	
```

# Gotchas
* [REMINDER] There are a fair amount of possible gotchas in a setup; the complexity ranges from simple to annoying, e.g.:
  - /usr/sbin/sshd **will NOT** tell you it's failing due to not being called by root >_> Fun way to lose 30 minutes
  - If you're doing the above as well as trying to be clever, e.g. setting names (not IPs) in known_hosts:
    - It /will/ work! Just keep tinkering until you get the host names matched up and you aren't asked to blindly trust the host.
      (I updated the readme to show that situation where it ended up working and I can `ssh Steve@r0ar` now :-D
  -
  
# Genereal Tricks & Troubleshooting
* Check out the ssh/d-related system logs:                        cat /var/log/system.log | grep sshd
* Check to see if there are any weird sshd processes:             ps auxww | grep 'sshd'
* Get unsigned fingerprint/pubkey of a key:                       ssh-keygen -l -f <pubkey>
* Search for any specific keys for a given host:                  ssh-keyscan -H 'localhost (::1)'; ssh-keycan -H 'host'
* Reset your ssh-agent env vars and process:                      eval $(ssh-agent -s)
* If you're still stuck, try turning on verbose logging:
	1. Try to launch your own sshd to confirm it's running:       sudo /usr/sbin/sshd -d
	2. Stop the macos sshd launchctl/daemon:                      sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist
    3. Uncomment the logging config lines in step #5
	4. Start ssh manually:                                        sudo /usr/sbin/sshd -d
    5. Attempt to connect to yourself in a new term:              ssh you@thelocallibrary.local -v
	6. Hopefully it works? Kill the sshd proc with `c-c`.
    7. Turn the system sshd background service back on:           sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist

# NNnnnnggg Duude Nothing Has Worked and I Am Absolutely Distraught
* I had some odd flags in the following launchctl files if you're still stuck. I doubt this fixed anything though!
  * /System/Library/LaunchDaemons/ssh.plist [OR] /Library/LaunchDaemons/ssh.plist
* Commands like `ssh-add` or `ssh-copy-id` are kinda handy but if you're this far gone, take a break bro
