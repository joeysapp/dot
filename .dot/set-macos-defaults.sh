#!/usr/bin/env bash

# Pulled & Modified versions of:
# * https://github.com/mathiasbynens/dotfiles/blob/main/.macos
# * https://mths.be/macos

# close sys prefs as we're changing sys pref items here;
osascript -e 'tell application "System Preferences" to quit'
sudo -v
# Keep-alive: update existing `sudo` time stamp until `.macos` has finished
while true;
  do sudo -n true;
  sleep 60;
  kill -0 "$$" || exit;
done 2>/dev/null &

# Set a custom wallpaper image. `DefaultDesktop.jpg` is already a symlink, and
# all wallpapers are in `/Library/Desktop Pictures/`. The default is `Wave.jpg`.
# rm -rf ~/Library/Application Support/Dock/desktoppicture.db
# sudo rm -rf /System/Library/CoreServices/DefaultDesktop.jpg
# sudo ln -s /path/to/your/image /System/Library/CoreServices/DefaultDesktop.jpg

; echo '\n\n== Safety =='
; defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false      ; echo never try to save to iCloud
; defaults write com.apple.LaunchServices LSQuarantine -bool false                 ; echo disable unknown developer dialog

; echo '\n\n== Network =='
; defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true         ; echo enable airdrop over ethernet
; defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true   ; echo no .DS_Store on ssh
; defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true       ; echo no .DS_Store on usb

; echo '\n\n== Main configs - Keyboard / Devices / UX =='
; defaults write NSGlobalDomain InitialKeyRepeat -int 15                         ; echo faster initial keypress
; defaults write NSGlobalDomain KeyRepeat -int 2                                 ; echo faster key repeating on hold
; defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false             ; echo disable any key-hold ux for key repeat
; defaults write com.apple.screencapture location -string "${HOME}/Desktop"      ; echo screenshot location set to Desktop
; defaults write com.apple.screencapture disable-shadow -bool true               ; echo disable shadows in screenshots
; defaults write com.apple.screencapture type -string "png"                      ; echo screenshot type=png
# ; defaults write com.apple.screencapture type -string "bmp"                      ; echo screenshot type bmp
# ; defaults write com.apple.screencapture type -string "gif"                      ; echo screenshot type gif
# ; defaults write com.apple.screencapture type -string "pdf"                      ; echo screenshot type pdf      ; lol, save screenshot as pdf
# ; defaults write com.apple.screencapture type -string "tiff"                     ; echo screenshot type tiff
; launchctl unload -w /System/Library/LaunchAgents/com.apple.rcd.plist 2> /dev/null ; echo prevent itunes from responding to keyboard keys
; sudo nvram SystemAudioVolume=" " ; echo no BOOM on boot
; defaults write com.apple.universalaccess reduceTransparency -bool true           ; echo disable menu transparency
; defaults write NSGlobalDomain NSWindowResizeTime -float 0.001                    ; echo cocoa apps resize immediately
; defaults write com.apple.terminal FocusFollowsMouse -bool true                   ; echo enable 'focus follows mouse' in terminal and all X11 apps
; defaults write org.x.X11 wm_ffm -bool true                                       ; echo (e.g. hover over a window and start typing in it)
; defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1             ; echo enable tap-to-click on trackpad here
; defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1                          ; echo enable tap-to-click on trackpad
; defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true     ; echo enable tap-to-click on bt mice
; defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true ; echo enable ctrl-scroll to zoom
; defaults write com.apple.universalaccess HIDScrollZoomModifierMask -int 262144 ; echo enable ctrl-scroll to zoom
; defaults write com.apple.universalaccess closeViewZoomFollowsFocus -bool true  ; echo enable ctrl-scroll focus follow
; defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false     ; echo disable typing auto caps
; defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false   ; echo disable typing dash sub
; defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false ; echo disable typing period sub
; defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false  ; echo disable typing quote sub
; defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false ; echo disable typing spell correction
; defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40             ; echo increase bluetooth bitrate/quality
; defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true  ; echo enable >1080p over hdmi (requires restart)
; sudo ln -sf "/Applications/Xcode.app/Contents/Developer/Applications/Simulator.app" "/Applications/Simulator.app"                   ; echo 'symlink iOS sim to Applications'
; sudo ln -sf "/Applications/Xcode.app/Contents/Developer/Applications/Simulator (Watch).app" "/Applications/Simulator (Watch).app"   ; echo 'symlink Watch sim to Apps'






; echo '\n\n== Energy settings'
; sudo pmset -a lidwake 1                                                        ; echo enable lid waking up laptop
; sudo pmset -a disablesleep 1                                                   ; echo turn off sleep, so closing lid does not put to sleep.
; sudo pmset -c sleep 0                                                          ; echo sleep never while charging
; sudo pmset -b sleep 5                                                          ; echo sleep in 5 mins on battery
; sudo pmset -a standbydelay 86400                                               ; echo standby delay to 24 hours, default is 1
; sudo systemsetup -setcomputersleep Off > /dev/null                             ; echo never go into computer sleep mode
; sudo pmset -a hibernatemode 0                                                  ; echo hibernatemode=0, never go into hiberation, speeds up sleep
; sudo pmset -a hibernatemode 3                                                  ; echo hibernatemode=3, copy ram to disk so state can restore if power fails
; sudo rm /private/var/vm/sleepimage                                             ; echo sleepimage - remove sleep image file to save disk space
; sudo touch /private/var/vm/sleepimage                                          ; echo sleepimage - new file at /private/var/vm/sleepimage
; sudo chflags uchg /private/var/vm/sleepimage                                   ; echo sleepimage - change flags to uchg
; defaults write com.apple.screensaver askForPassword -bool true                 ; echo password req from sleep/screensaver
; defaults write com.apple.screensaver askForPasswordDelay -bool false           ; echo password req immediately
; defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true         ; echo do not automatically close inactive apps







; echo '\n\n== Hot Corners'
# Hot corners
# Possible values:
#  0: no-op
#  2: Mission Control
#  3: Show application windows
#  4: Desktop
#  5: Start screen saver
#  6: Disable screen saver
#  7: Dashboard
# 10: Put display to sleep
# 11: Launchpad
# 12: Notification Center
# 13: Lock Screen
# Top left screen corner → Mission Control
defaults write com.apple.dock wvous-tl-corner -int 2
defaults write com.apple.dock wvous-tl-modifier -int 0
# Top right screen corner → Desktop
defaults write com.apple.dock wvous-tr-corner -int 4
defaults write com.apple.dock wvous-tr-modifier -int 0
# Bottom left screen corner → Start screen saver
defaults write com.apple.dock wvous-bl-corner -int 5
defaults write com.apple.dock wvous-bl-modifier -int 0





; echo '\n\n== Apps'
; defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true    ; echo Finder - open window on volume mount
; defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true       ; echo Finder - open window on volume mount
; defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"            ; echo Finder - open with list view default
; defaults write com.apple.finder NewWindowTarget -string "PfHm"                 ; echo Finder - open with location $HOME
# ; defaults write com.apple.finder NewWindowTarget -string "PfCm"                 ; echo default location Computer
# ; defaults write com.apple.finder NewWindowTarget -string "PfVo"                 ; echo default location Volume
# ; defaults write com.apple.finder NewWindowTarget -string "PfDe"                 ; echo default location Desktop
# ; defaults write com.apple.finder NewWindowTarget -string "PfDo"                 ; echo default location Documents
# ; defaults write com.apple.finder NewWindowTarget -string "PfAF"                 ; echo default location All My Files
# ; defaults write com.apple.finder NewWindowTarget -string "PfLo"                 ; echo default location Other...
# ; defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"  ; echo default location
; defaults write com.apple.finder _FXSortFoldersFirst -bool true                 ; echo Finder - place folders on top during sort
; defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"            ; echo Finder - search current path by default
; defaults write com.apple.finder QuitMenuItem -bool true                        ; echo Finder - allow command-q for restarting
; defaults write com.apple.finder DisableAllAnimations -bool true                ; echo Finder - disable all animations
; defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false     ; echo Finder - remove warning on changing a file extension   
; defaults write com.apple.finder WarnOnEmptyTrash -bool false                   ; echo Finder - remove warning on emptying trash
; defaults write com.apple.finder AppleShowAllFiles -bool true                   ; echo Finder - show hidden files in finder
; defaults write NSGlobalDomain AppleShowAllFiles -bool true                     ; echo Finder - show hidden files in all dialogs
; defaults write NSGlobalDomain AppleShowScrollBars -string "Always"             ; echo Finder - show scrollbars always
; defaults write NSGlobalDomain AppleShowAllExtensions -bool true                ; echo Finder - show extensions always
; defaults write com.apple.finder ShowStatusBar -bool true                       ; echo Finder - show status bar on bottom - file count/size
; defaults write com.apple.finder ShowPathbar -bool true                         ; echo Finder - show path on bottom
; defaults write com.apple.finder _FXShowPosixPathInTitle -bool true             ; echo Finder - show path in title
; chflags nohidden ~/Library                                                     ; echo Finder - show ~/Library
; xattr -d com.apple.FinderInfo ~/Library                                        ; echo Finder - show ~/Library - xattr
; chflags nohidden /Volumes                                                      ; echo Finder - show /Volumes
; defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true         ; echo Finder - show desktop all media icons
; defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true     ; echo Finder - show desktop usb icons
; defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true             ; echo Finder - show desktop volume icons
; defaults write com.apple.finder ShowMountedServersOnDesktop -bool true         ; echo Finder - show desktop server icons
;                                                                                ; echo Finder - all Get Info panels expanded automatically
; defaults write com.apple.finder FXInfoPanesExpanded -dict \                    
         General -bool true \
         MoreInfo -bool true \
         MetaData -bool true \
         Preview -bool true \
         OpenWith -bool true \
         Privileges -bool true \
         Comments -bool true
; defaults write com.apple.commerce AutoUpdate -bool true                          ; echo System/* - enable Auto Update
; defaults write com.apple.CrashReporter DialogType -string "none"                 ; echo System/CrashReporter - disable apple CrashReporter
; defaults write com.apple.helpviewer DevMode -bool true                           ; echo System/HelpViewer - window in non-floating mode
; defaults write NSGlobalDomain AppleKeyboardUIMode -int 3                         ; echo System/All Dialogs - full kb access, eg tab
; defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true      ; echo System/SaveDialog - panel 1 always expanded
; defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true     ; echo System/SaveDialog - panel 2 always expanded
; defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true         ; echo System/PrintDialog - panel 1 always expanded
; defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true        ; echo System/PrintDialog - panel 2 always expanded
; defaults write com.apple.terminal StringEncodings -array 4                       ; echo Terminal - only use utf-8 in Terminal.app
; defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true     ; echo Print - app auto close on finish
; defaults write com.apple.dock mouse-over-hilite-stack -bool true                 ; echo Dock - mouse over on stacks in dock
; defaults write com.apple.dock launchanim -bool false                             ; echo Dock - do not animate opening apps
; defaults write com.apple.dock autohide-delay -float 0                            ; echo Dock - do not delay for auto-hiding
; defaults write com.apple.dock autohide-time-modifier -float 0                    ; echo Dock - do not animate hiding
; defaults write com.apple.dock tilesize -int 36                                   ; echo Dock - set dock icon size to 36x36px
; defaults write com.apple.dock mineffect -string "scale"                          ; echo Dock - minimize effect is scale not genie
; defaults write com.apple.dock minimize-to-application -bool false                ; echo Dock - minimize to left side of dock
; defaults write com.apple.dock show-process-indicators -bool true                 ; echo Dock - always show dot for open apps
; defaults write com.apple.dock persistent-apps -array                             ; echo Dock - remove stock apps from dock
; defaults write com.apple.dock showhidden -bool true                              ; echo Dock - make hidden apps translucent
; defaults write com.apple.dock persistent-apps -array-add '{tile-data={}; tile-type="spacer-tile";}';     ; echo Dock - add spacer on left (where apps are)
; defaults write com.apple.dock persistent-others -array-add '{tile-data={}; tile-type="spacer-tile";}'    ; echo Dock - add spacer on right (where trash is)
; defaults write com.apple.commerce AutoUpdateRestartRequired -bool true           ; echo App Store - allow App Store to reboot on macOS updates
; defaults write com.apple.appstore WebKitDeveloperExtras -bool true               ; echo App Store - enable WebKit dev tools
; defaults write com.apple.appstore ShowDebugMenu -bool true                       ; echo App Store - enable debug menu
; defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true         ; echo Software Update - enable automatic update check
; defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1                 ; echo Software Update - check daily, not once a week
; defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1                 ; echo Software Update - download automatically in background
; defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1             ; echo Software Update - automatically update system data files/security updates
; defaults write com.apple.SoftwareUpdate ConfigDataInstall -int 1                 ; echo Software Update - automatically download iCloud apps
; defaults write com.apple.ActivityMonitor OpenMainWindow -bool true               ; echo Activity Monitor - show main window on launch
; defaults write com.apple.ActivityMonitor IconType -int 5                         ; echo Activity Monitor - show CPU activity in dock icon
; defaults write com.apple.ActivityMonitor ShowCategory -int 0                     ; echo Activity Monitor - show all processes
; defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"           ; echo Activity Monitor - sort by CPU usage
; defaults write com.apple.ActivityMonitor SortDirection -int 0                    ; echo Activity Monitor - sort direction
# ; launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null ; echo NotificationCenter - disable and remove menu bar icon
; defaults write org.m0k.transmission IncompleteDownloadFolder -string "${HOME}/Downloads/Torrents"    ; echo Transmission - folder set to /Downloads/Torrents
; defaults write org.m0k.transmission UseIncompleteDownloadFolder -bool true                           ; echo Transmission - save incomplete files there
; defaults write org.m0k.transmission DownloadLocationConstant -bool true                              ; echo Transmission - save complete files there
; defaults write org.m0k.transmission DownloadAsk -bool false                                          ; echo Transmission - do not confirm on open
; defaults write org.m0k.transmission MagnetOpenAsk -bool false                                        ; echo Transmission - do not confirm on magnet open 
; defaults write org.m0k.transmission CheckRemoveDownloading -bool true                                ; echo Transmission - do not prompt on deletion 
; defaults write org.m0k.transmission DeleteOriginalTorrent -bool false                                ; echo Transmission - do not delete transmission file
; defaults write org.m0k.transmission WarningDonate -bool false                                        ; echo Transmission - hide donate msg
; defaults write org.m0k.transmission WarningLegal -bool false                                         ; echo Transmission - hide legal disclaimer
; defaults write org.m0k.transmission RandomPort -bool true                                            ; echo Transmission - random port on launch
; defaults write org.m0k.transmission BlocklistNew -bool true                                          ; echo Transmission - use a block list
; defaults write org.m0k.transmission BlocklistAutoUpdate -bool true                                   ; echo Transmission - block list auto update
# Source: https://giuliomac.wordpress.com/2014/02/19/best-blocklist-for-transmission/
#; defaults write org.m0k.transmission BlocklistURL -string "http://john.bitsurge.net/public/biglist.p2p.gz"




; echo '\n\n== Unknown / Broken / nonfunctional'
; defaults write com.apple.dock enable-spring-load-actions-on-all-items -bool true ; echo spring loading all Dock items
; defaults write NSGlobalDomain com.apple.springing.enabled -bool true             ; echo spring loading all directories
; defaults write NSGlobalDomain com.apple.springing.delay -float 0                 ; echo spring loading all directories
; defaults write com.apple.frameworks.diskimages skip-verify -bool true            ; echo disable disk image verification
; defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true     ; echo disable disk image verification
; defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true     ; echo disable disk image verification 
; defaults write com.apple.dock expose-animation-duration -float 0.1               ; echo Mission Control - broken speed up animation
; defaults write com.apple.dock expose-group-by-app -bool false                    ; echo Mission Control - broken
; defaults write com.apple.dock expose-group-apps -bool false                      ; echo Mission Control - broken
; defaults write com.apple.dashboard mcx-disabled -bool true                       ; echo Dashboard - disable dashboard
; defaults write com.apple.dock dashboard-in-overlay -bool true                    ; echo Dashboard - dont show dashboard as a space
; defaults write com.apple.dock mru-spaces -bool false                             ; echo Dashboard - do not automatically rearrange Spaces based on most recent use



echo '\n\n== Spotlight re-ordering / re-indexing'
#; sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search         ; echo Spotlight - hide tray-icon (and subsequent helper)
; sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array "/Volumes"    ; echo Spotlight - disable indexing on mounted volumes
; sudo mdutil -i off "/Volumes/foo"                                                        ; echo Spotlight - eg stop indexing in /Volumes/foo
# 	MENU_SPOTLIGHT_SUGGESTIONS (send search queries to Apple)
# 	MENU_WEBSEARCH             (send search queries to Apple)
defaults write com.apple.spotlight orderedItems -array \
	'{"enabled" = 1;"name" = "APPLICATIONS";}' \
	'{"enabled" = 1;"name" = "SYSTEM_PREFS";}' \
	'{"enabled" = 1;"name" = "DIRECTORIES";}' \
	'{"enabled" = 1;"name" = "PDF";}' \
	'{"enabled" = 1;"name" = "FONTS";}' \
	'{"enabled" = 0;"name" = "DOCUMENTS";}' \
	'{"enabled" = 0;"name" = "MESSAGES";}' \
	'{"enabled" = 0;"name" = "CONTACT";}' \
	'{"enabled" = 0;"name" = "EVENT_TODO";}' \
	'{"enabled" = 0;"name" = "IMAGES";}' \
	'{"enabled" = 0;"name" = "BOOKMARKS";}' \
	'{"enabled" = 0;"name" = "MUSIC";}' \
	'{"enabled" = 0;"name" = "MOVIES";}' \
	'{"enabled" = 0;"name" = "PRESENTATIONS";}' \
	'{"enabled" = 0;"name" = "SPREADSHEETS";}' \
	'{"enabled" = 0;"name" = "SOURCE";}' \
	'{"enabled" = 0;"name" = "MENU_DEFINITION";}' \
	'{"enabled" = 0;"name" = "MENU_OTHER";}' \
	'{"enabled" = 0;"name" = "MENU_CONVERSION";}' \
	'{"enabled" = 0;"name" = "MENU_EXPRESSION";}' \
	'{"enabled" = 0;"name" = "MENU_WEBSEARCH";}' \
	'{"enabled" = 0;"name" = "MENU_SPOTLIGHT_SUGGESTIONS";}'
; killall mds > /dev/null 2>&1                  ; echo Spotlight - load new settings before rebuilding index
; sudo mdutil -i on / > /dev/null               ; echo Spotlight - ensure indexing is enabled for main volumne
; sudo mdutil -E / > /dev/null                  ; echo Spotlight - rebuild index


# Enable the debug menu in Address Book
defaults write com.apple.addressbook ABShowDebugMenu -bool true

# Enable Dashboard dev mode (allows keeping widgets on the desktop)
defaults write com.apple.dashboard devmode -bool true

# Enable the debug menu in iCal (pre-10.8)
defaults write com.apple.iCal IncludeDebugMenu -bool true

# Use plain text mode for new TextEdit documents
defaults write com.apple.TextEdit RichText -int 0
# Open and save files as UTF-8 in TextEdit
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

# Enable the debug menu in Disk Utility
defaults write com.apple.DiskUtility DUDebugMenuEnabled -bool true
defaults write com.apple.DiskUtility advanced-image-options -bool true

# Auto-play videos when opened with QuickTime Player
defaults write com.apple.QuickTimePlayerX MGPlayMovieOnOpen -bool true

###############################################################################
# Mac App Store                                                               #
###############################################################################

# Enable the WebKit Developer Tools in the Mac App Store
defaults write com.apple.appstore WebKitDeveloperExtras -bool true

# Enable Debug Menu in the Mac App Store
defaults write com.apple.appstore ShowDebugMenu -bool true

# Enable the automatic update check
defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true

# Check for software updates daily, not just once per week
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# Download newly available updates in background
defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1

# Install System data files & security updates
defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1

# Automatically download apps purchased on other Macs
defaults write com.apple.SoftwareUpdate ConfigDataInstall -int 1

# Turn on app auto-update
defaults write com.apple.commerce AutoUpdate -bool true

# Allow the App Store to reboot machine on macOS updates
#defaults write com.apple.commerce AutoUpdateRestartRequired -bool true

; defaults write com.apple.commerce AutoUpdateRestartRequired -bool true       ; echo All Apps - allow App Store to reboot on macOS updates
; defaults write com.apple.commerce AutoUpdateRestartRequired -bool true       ; echo All Apps - allow App Store to reboot on macOS updates


echo '\n\n== Done =='
for app in "Activity Monitor" \
	"Address Book" \
	"Calendar" \
	"cfprefsd" \
	"Contacts" \
	"Dock" \
	"Finder" \
	"Mail" \
	"Messages" \
	"Photos" \
	"Safari" \
	"SystemUIServer" \
	"Terminal" \
	"Transmission" \
	"iCal"; do
	killall "${app}" &> /dev/null
done
echo "Done. Note that some of these changes require a logout/restart to take effect."



### Unused/Extras

###############################################################################
# Safari & WebKit                                                             #
###############################################################################

# Privacy: don’t send search queries to Apple
defaults write com.apple.Safari UniversalSearchEnabled -bool false
defaults write com.apple.Safari SuppressSearchSuggestions -bool true

# Press Tab to highlight each item on a web page
defaults write com.apple.Safari WebKitTabToLinksPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2TabsToLinks -bool true

# Show the full URL in the address bar (note: this still hides the scheme)
defaults write com.apple.Safari ShowFullURLInSmartSearchField -bool true

# Set Safari’s home page to `about:blank` for faster loading
defaults write com.apple.Safari HomePage -string "about:blank"

# Prevent Safari from opening ‘safe’ files automatically after downloading
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false

# Allow hitting the Backspace key to go to the previous page in history
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2BackspaceKeyNavigationEnabled -bool true

# Hide Safari’s bookmarks bar by default
defaults write com.apple.Safari ShowFavoritesBar -bool false

# Hide Safari’s sidebar in Top Sites
defaults write com.apple.Safari ShowSidebarInTopSites -bool false

# Disable Safari’s thumbnail cache for History and Top Sites
defaults write com.apple.Safari DebugSnapshotsUpdatePolicy -int 2

# Enable Safari’s debug menu
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true

# Make Safari’s search banners default to Contains instead of Starts With
defaults write com.apple.Safari FindOnPageMatchesWordStartsOnly -bool false

# Remove useless icons from Safari’s bookmarks bar
defaults write com.apple.Safari ProxiesInBookmarksBar "()"

# Enable the Develop menu and the Web Inspector in Safari
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true

# Add a context menu item for showing the Web Inspector in web views
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

# Enable continuous spellchecking
defaults write com.apple.Safari WebContinuousSpellCheckingEnabled -bool true
# Disable auto-correct
defaults write com.apple.Safari WebAutomaticSpellingCorrectionEnabled -bool false

# Disable AutoFill
defaults write com.apple.Safari AutoFillFromAddressBook -bool false
defaults write com.apple.Safari AutoFillPasswords -bool false
defaults write com.apple.Safari AutoFillCreditCardData -bool false
defaults write com.apple.Safari AutoFillMiscellaneousForms -bool false

# Warn about fraudulent websites
defaults write com.apple.Safari WarnAboutFraudulentWebsites -bool true

# Disable plug-ins
defaults write com.apple.Safari WebKitPluginsEnabled -bool false
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2PluginsEnabled -bool false

# Disable Java
defaults write com.apple.Safari WebKitJavaEnabled -bool false
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2JavaEnabled -bool false
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2JavaEnabledForLocalFiles -bool false

# Block pop-up windows
defaults write com.apple.Safari WebKitJavaScriptCanOpenWindowsAutomatically -bool false
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2JavaScriptCanOpenWindowsAutomatically -bool false

# Disable auto-playing video
# defaults write com.apple.Safari WebKitMediaPlaybackAllowsInline -bool false
# defaults write com.apple.SafariTechnologyPreview WebKitMediaPlaybackAllowsInline -bool false
# defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2AllowsInlineMediaPlayback -bool false
# defaults write com.apple.SafariTechnologyPreview com.apple.Safari.ContentPageGroupIdentifier.WebKit2AllowsInlineMediaPlayback -bool false

# Enable "Do Not Track"
defaults write com.apple.Safari SendDoNotTrackHTTPHeader -bool true

# Update extensions automatically
defaults write com.apple.Safari InstallExtensionUpdatesAutomatically -bool true

###############################################################################
# Photos                                                                      #
###############################################################################

# Prevent Photos from opening automatically when devices are plugged in
defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true

###############################################################################
# Messages                                                                    #
###############################################################################

# Disable automatic emoji substitution (i.e. use plain text smileys)
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticEmojiSubstitutionEnablediMessage" -bool false

# Disable smart quotes as it's annoying for messages that contain code
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticQuoteSubstitutionEnabled" -bool false

# Disable continuous spell checking
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "continuousSpellCheckingEnabled" -bool false


###############################################################################
# Transmission.app                                                            #
###############################################################################
