# ssh
Information for correct ssh/daemon setup, using passwordless pubkey generated client-side meaning you can use git without having to type a password every pull or submodule update.

# Howto
These are the things you need to configure to have the daemon and client interact nicely.

First, create two keys - one for you as the client, and one for you as the host (sshd)
```
$ ssh-keygen -t <algo> -b 4096 -C <comment> -> (client_key, client_key.pub)
$ ssh-keygen -t <algo> -b 4096 -C <comment> -> (host_key, host_key.pub)
```
You may need to activate the client key with `ssh-add ~/.../client_key`.

Now set up the following file structure (you will need to create some, e.g. `__known_hosts`)
```
    Client
    Users/<user>/.ssh/ (this repo)
    Users/<user>/.ssh/config
        - File containing a user-specific (overriding root config) list of:
          Host ?nameofhost?
              HostNames ?ipofhost?
              User <user>
              IdentityFile <path of private key>
    Users/<user>/.ssh/known_hosts
        - File containing known daemons public keys (signed/masked) (ONLY after yes/no/fp)
        127.0.0.1 ssh-rsa AAAAAACCCC..........+RR/
    Users/<user>/.ssh/client_key
    Users/<user>/.ssh/client_key.pub

    Daemon
    </etc/ssh or private/etc/ssh, on macos they are symlinked>
    etc/ssh/ssh_config
        - Config file for global client use (lots of stuff)
    etc/ssh/sshd_config
        - Config file for daemon (lots of stuff)
          AuthorizedKeysFile /etc/ssh/__known_clients
          LogLevel DEBUG3
          LogVerbose kex.c:*:1000,*:kex_exchange_identification():*,packet.c:*
          PasswordAuthentication no
          PubkeyAuthentication yes # This is default yes, but be explicit
          HostKey /etc/ssh/my_cool_private_daemon_key
    etc/ssh/__known_clients
        ssh-rsa AAAAACC......CCMM+m client_key
    etc/ssh/daemon_key
    etc/ssh/daemon_key.pub
```

# Tips
* (TBD) If the default sshd LaunchDaemon's <Disabled true> actually mattered or not
  * /System/Library/LaunchDaemons/ssh.plist vs
  * /Library/LaunchDaemons/ssh.plist (manually added, not using atm)
* Commands like `ssh-add` or `ssh-copy-id` are nice, but you mainly just need the above file structure.
* Reset your ssh-agent env vars and process:
`eval $(ssh-agent -s)`
* Check ps auxww | grep 'sshd'
* Get unsigned fingerprint/pubkey of a key:
`ssh-keygen -l -f <pubkey>`
* Search for any specific keys for a given host:
`ssh-keyscan -H 'localhost (::1)'; ssh-keycan -H 'host'`
* Look at any past ssh events:
`cat /var/log/system.log | grep sshd`
* Trouble shooting with verbose logging:
	1. Try to launch your own sshd to confirm it's running:
	`/usr/sbin/sshd -d`
	2. Stop the macOS SSHD service running from this LaunchDaemon:
	`sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist`
	3. Load back up your own sshd with LogLevel DEBUG3 and specific string formatting, then:
	`/usr/sbin/sshd -d`
	`ssh user@host -v`
	4. Confirm it's all working, kill it with Ctrl-C
	5. Load back up the normal daemon
``sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist`
