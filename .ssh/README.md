# ssh
How to setup either side of a key-exchange ssh auth flow, e.g. you need to follow these steps to both:
  1. Connect to remotes with your brand new pubkey as the "client" 
  2. Allow remotes to connect to you with their pubkeys as the "host"

## Step #0 - Generate the client keypair if you don't have a vast collection already
Generate a new client public/private key pair for connecting to remote hosts:
```zsh
$ ssh-keygen -t <algo> -b 4096 -C "client-keypair"
  -> client_key, client_key.pub are added to ~/.ssh
  
  If you have trouble connecting, you can run the following, but it wasn't necessary for me:
$ ssh-add ~/.../client_key`.
```

## Step #1 - Generate the host keypair and hand it over to root
Generate a new host public/private key pair for accepting client connections, and give ownership to root. I assume moving to the system ssh location isn't necessary, but if you don't, remember that when you're editing the host config files later.
```zsh
$ ssh-keygen -t <algo> -b 4096 -C "host-keypair"
   -> host_key, host_key.pub are added to ~/.ssh
   
$ mv host_key host_key.pub /etc/ssh/
$ sudo chown root /etc/ssh/host_key /etc/ssh/host_key.pub
```

## Step #2 - Add your new client private key to your outbound config
You're going to now add: the new client private key, the intended host, and the client's username to your local config.
```
~/.ssh/config
    Host                  name-of-host
        HostName          ip-address-of-host
        User              whoever-you-gonna-connect-as
        IdentityFile      path-to-new-client-private-key
```

## Step #2A (Optional) - Add your private key to your client's outbound config
You can use add yourself as an intended host, allowing you to connect to yourself. My use case for this was a script that watched a submodule across several local repos. Did not want to enter a password every update.
```
~/.ssh/config
    Host                  127.0.0.1
        HostName          127.0.0.1
        User              Steve Bizzblejj
        IdentityFile      /Users/Craig_Levinson/.ssh/client_key
```

## Step #3 - Add (pre-existing) remote host public key(s) to your client's known public keys
You now need to get your host's public key (either your own, downloaded, handed it via 512mb thumb drive in a dark alley), then add it to your client known host file.
**[WARN]** If you are creating your host keys on a remote machine, I had some issues with keygens letting you skip `Y/N/FP` - the keys will be invalid without that interaction.
```
$ cd ~/Downloads
$ unzip ./federal_tax_returns-Steve.zip
$ cat ./federal_tax_returns-Steve/Nicaragua_trip_1993_host_key.pub >> ~/.ssh/known_hosts
$ tail -n 1 ~/.ssh/known_hosts

name-of-external-host name-of-key-cipher AAAAAAAAAAAAAAAAAARRRRRRRRRRRRRRGGGGGGGHHHHHHHHHHHH
```

## Step #3A (Optional part 2) - Add (your brand new) local host public key to your client's known public keys
Step 2A continued: To finish setting up passwordless git/ssh for local development, take your new host public key (or any public key you have access to the private key to I guess, but that isn't the point of this README, it's for me to read in 19 years and complain at past me.)
```
$ sudo cat /etc/ssh/host_key >> ~/.ssh/known_hosts
$ tail -n 1 ~/.ssh/known_hosts
    127.0.0.1 TicTacToe YEEEEEEEEEEEEEEEEEEEEEOWWWWWWWWWWWWWWWWWWWWWWWCH!!!!!!!!!!!!!
```


## Step #4 - Waltz over to the desired host (or remote into with a password but that's boring)


## Step #4 - Create a new file to add your client's public key
I personally just made a new file on the host machine so I could keep track and not edit a single file in the future. If you made it this far you can do either way:
**[NOTE]**: (I realized that `/etc/ssh` and `private/etc/ssh` are symlinks somewhat late at night the first time doing this :-0)

```bash
$ touch /etc/ssh/trusted_incoming_clients
$ cat ~/.ssh/client_key.pub >> /etc/ssh/trusted_incoming_clients
```

## Step #5 - Add the below to the host config file which identifies the client's public key
If you didn't move the private host key generated in step #1, use that location.
```
/etc/ssh/sshd_config
    AuthorizedKeysFile /etc/ssh/trusted_incoming_clients
    HostKey /etc/ssh/host_key
    # This should be yes by default, but I like being explicit
    PubkeyAuthentication yes
    PasswordAuthentication no
    # If you're having issues, remove these and watch sshd's logs
    # LogLevel DEBUG3
    # LogVerbose kex.c:*:1000,*:kex_exchange_identification():*,packet.c:*
```

## Step #6 - Restart sshd, don't test anything, go play Pokeman
```bash
$ sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist
$ logout
```

# Tricks / Troubleshooting
* Get unsigned fingerprint/pubkey of a key:                       `ssh-keygen -l -f <pubkey>`
* Search for any specific keys for a given host:                  `ssh-keyscan -H 'localhost (::1)'; ssh-keycan -H 'host'`
* Check out the ssh/d-related system logs:                        `cat /var/log/system.log | grep sshd`
* Reset your ssh-agent env vars and process:                      `eval $(ssh-agent -s)`
* Check to see if there are any weird sshd processes:             `ps auxww | grep 'sshd'`
* If you're still stuck, try turning on verbose logging:
	1. Try to launch your own sshd to confirm it's running:       `/usr/sbin/sshd -d`
	2. Stop the macos sshd launchctl/daemon:                      `sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist`
    3. Uncomment the logging config lines in step #5
	4. Start ssh manually:                                        `/usr/sbin/sshd -d`
    5. Attempt to connect to yourself in a new term:              `ssh you@the_local_library.local -v`
	6. Hopefully it works? Kill the sshd proc with `c-c`.
    7. Turn the system sshd background service back on:           `sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist`

# NNnnnnggg Duude Nothing Has Worked and I Am Absolutely Distraught
* I had some odd flags in the following launchctl files if you're still stuck. I doubt this fixed anything though!
  * /System/Library/LaunchDaemons/ssh.plist [OR] /Library/LaunchDaemons/ssh.plist
* Commands like `ssh-add` or `ssh-copy-id` are kinda handy but if you're this far gone, take a break bro
