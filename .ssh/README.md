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
- [ ] 6. [Host, Kblob] Kittenblob "adds" that file to `sshd_config` and their own privkey: 
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
