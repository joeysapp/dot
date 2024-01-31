   	 Instructions - [ FOLLOW TO A TEE. DOUBLE CHECK STEPS. IT IS EASY TO MESS UP. ]
            Use these emojis to make sure:
        ✅✅✅✅❌❌❌❌
   	 These are the required steps from either side, with existing keys. 
   	
    
    
   	 = "The Pubkey exchange between host and client" ============================
   ❌	 1. [Host, Kblob] Kittenblob sends us their host pubkey file                    (k:/etc/ssh/kittenblob_host_edd.pub) -> f:~/.ssh/
   ❌	 2. [Client,  us] We send Kittenblob our client pubkey file                           (f:~/.ssh/foxy_client_edd.pub) -> k:/etc/ssh/
   	
   	
   	 = "The Client config " ========================================
   ❌	 3. [Client,  us] We "add" Kittenblob and their details to our config                              (f:~/.ssh/config)
   ❌	     * "Host" is either 'kittenblob', 'kittenblob.local' or.. just their local IP if all else fails.
   ❌	     * "HostName" is kittenblob's internal IP
   ❌	     * "User" is zooey
   ❌	     * "IdentityFile" is our (Foxy) private client key file                                 (f:~/.ssh/foxy_client_edd)
   	
   	
   	 = "The Host setup and config (TWO STEPS)" ========================================
   ❌	 3. [Host, Kblob] (1st time) Kittenblob makes a "KoolClients" file and adds it to:           (k:/etc/ssh/sshd_config)
   ❌	 4. [Host, Kblob] Kittenblob adds our (Foxy's) public key to KoolClients                     (k:/etc/ssh/KoolClients)
   ❌	      * That pubkey was sent by us, the client, to Kittenblob the host in step #2.
   	
