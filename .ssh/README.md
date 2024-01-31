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
    **[WARN]**
    If you are creating your host keys on a remote machine, I had issues with keygens letting you skip `Y/N/FP`..
    I'm not entirely sure and that was over 2 years ago. I was probably doing this process incorrectly. Shrug

```zsh
$ ssh-keygen -t <algo> -b 4096 -C "host-keypair"
   -> host_key, host_key.pub are added to ~/.ssh
   
$ mv host_key host_key.pub /etc/ssh/
$ sudo chown root /etc/ssh/host_key /etc/ssh/host_key.pub
```

## Step #2 - [CLIENT] Add your new client private key to your outbound config
You're going to now add: the new client private key, the intended host, and the client's username to your local config.
    [INFO]
    You likely won't have this file on a new installation. You can copy /etc/ssh/ssh_config, or just add these 4 lines.
```
~/.ssh/config
    Host                  (what-you-type-in-after-the-@) (its gotta be in a few spots, keep reading!)
        HostName          ip-address-of-host
        User              whoever-you-gonna-connect-as
        IdentityFile      path-to-new-client-private-key
```

## Step #2A (Optional) - [CLIENT] Add your private key to your client's outbound config
You can use add yourself as an intended host, allowing you to connect to yourself. My use case for this was a script that watched a submodule across several local repos. Did not want to enter a password every update.
```
~/.ssh/config
    Host                  r0ar                    <-- Pay attention to how you get this to work!
        HostName          127.0.0.1               <-- This too!
        User              Stan
        IdentityFile      /Users/Craig_Levinson/.ssh/client_key
```

## Step #3 - [CLIENT] Add (pre-existing) remote host public key(s) to your client's known public keys
You now need to get your host's public key (either your own, downloaded, handed it via 512mb thumb drive in a dark alley), then add it to your client known host file.
**[NOTE FOR CUSTOM HOSTNAMES]**
    - If you set a custom Host value in the above step, _MAKE SURE_ it matches in these three locations:

```
$ cd ~/Downloads
$ unzip ./federal_tax_returns.zip
$ cat ./federal_tax_returns/Nicaragua_host_key.pub >> ~/.ssh/known_hosts
$ tail -n 1 ~/.ssh/known_hosts
   -> name-of-external-host name-of-key-cipher AAAAAAAAAAAAAAAAAARRRRRRRRRRRRRRGGGGGGGHHHHHHHHHHHH

 [NOTE]
    To have a cool 'custom' Host value, the pubkey you just wrote to .ssh/known_hosts _HAS_ to have that IP at the start of it:
    $ 127.0.0.1 name-of-key-cipher AAA...
    .. You can manually edit the known_hosts file.
    .. or try : `ssh-keygen -r 127.0.0.1` on the client's side at this point? Just do the following:

 [SUMMATION OF CUSTOM HOST STRING FOR SSH]
 * Host, HostName (and Name?) MUST be identical in these ..TWO? files:
   Client: ~/.ssh/config:            Host=r0ar, HostName=127.0.0.1,   User: zooey
   Client: ~/.ssh/known_hosts:       127.0.0.1 MUST be first, before the pubkey's cipher. (can be manually added)
       ....
   I thought somewhere on the host had to have r0ar/127.0.0.1, but at least for just accepting the client, I think
   you just need the above two files... OH! GIT!

  [CUSTOM HOST STRINGS FOR GIT]
   You will have to update your git remote Host strings if you want to do passwordless git usage, e.g.:
$ git remote set-url r0ar 
```

## Step #3A (Optional part 2) - [CLIENT] Add (your brand new) local host public key to your client's known public keys
Step 2A continued: To finish setting up passwordless git/ssh for local development, take your new host public key (or any public key you have access to the private key to I guess, but that isn't the point of this README, it's for me to read in 19 years and complain at past me.)
```
$ sudo cat /etc/ssh/host_key >> ~/.ssh/known_hosts
$ tail -n 1 ~/.ssh/known_hosts
    127.0.0.1 TicTacToeCipher YEEEEEEEEEEEEEEEEEEEEEOWWWWWWWWWWWWWWWWWWWWWWWCH!!!!!!!!!!!!!
```


## Step #4 - [GO TO HOST] Waltz over to the desired host (or remote into with a password but that's boring)


## Step #4 - [HOST] Create a new file to add your client's public key
I personally just made a new file on the host machine so I could keep track and not edit a single file in the future. If you made it this far you can do either way:
**[NOTE]**: (I realized that `/etc/ssh` and `private/etc/ssh` are symlinks somewhat late at night the first time doing this :-0)

```bash
$ touch /etc/ssh/trusted_incoming_clients
$ cat ~/.ssh/client_key.pub >> /etc/ssh/trusted_incoming_clients
```

## Step #5 - [HOST] Add the below to the host config file which identifies the client's public key
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

## Step #6 - [ENLIGHTENED PROTESTANT] Restart sshd, don't test anything, go play Pokeman
```bash
$ sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist
$ logout
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
