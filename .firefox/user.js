/*
   ~/.firefox/user.js  (or wherever your firefox profile is)
   
   [about]
   This file is read in on every firefox start and applies preferences set here.
   These preferences can be seen on `about:config` (type that into your address bar)
   This file is read _AFTER_ prefs.js (which is not per-user?)
  
   [notes]
   - Some preferences will not show until they are set ("hidden preferences")
   - true/false !== 1/0
  
   [refs]
   https://kb.mozillazine.org
   https://kb.mozillazine.org/Browser.bookmarks.file
   https://github.com/arkenfox/user.js/wiki/2.1-User.js
*/

// Override system theme (0=light, 1=dark)
user_pref("ui.systemUsesDarkTheme", 0);

// Allow use of customizing CSS on all pages (./chrome/userProfile.css and ./chrome/userContent.css)
use_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// Save bookmarks to file on exit (https://support.mozilla.org/en-US/questions/928809)
user_pref("browser.bookmarks.autoExportHTML", true);
user_pref("browser.bookmarks.file", "~/.firefox/bookmarks-backup.html");

// Turn off webps
user_pref('image.webp.enabled', false);

// Allow legacy alt-click to save links/images (handy when right-click is disabled)
user_pref('browser.altClickSave', true);

//              __
// .-----.----.|__|.--.--.---.-.----.--.--.
// |  _  |   _||  ||  |  |  _  |  __|  |  |
// |   __|__|  |__| \___/|___._|____|___  |
// |__|                             |_____|

// Limit font visibility that can be used in browser fingerprinting
// 1=Base system fonts
// 2=also fonts from optional language packs
// 3=also user-installed fonts
user_pref("layout.css.font-visibility.private", 1);
user_pref("layout.css.font-visibility.standard", 1);

user_pref("gfx.font_rendering.opentype_svg.enabled", false);

// Send DNT=1 in all requests
user_pref("layout.css.font-visibility.trackingprotection", 1);

/* 2601: prevent accessibility services from accessing your browser
 * [1] https://support.mozilla.org/kb/accessibility-services ***/
user_pref("accessibility.force_disabled", 1);

// Disable Location-Aware Browsing (geolocation)
// Enable hardening against various fingerprinting vectors (Tor Uplift project)
// Note this can break the ability to remember the window size of Firefox on startup
user_pref("privacy.resistFingerprinting", true);
        
// Enable first-party isolation
user_pref("privacy.firstparty.isolate", true);
  
// Disable WebRTC to prevent leaking inernal IP addresses
user_pref("media.peerconnection.enabled", false);
user_pref("media.navigator.enabled", false);
user_pref("media.navigator.video.enabled", false);
user_pref("media.getusermedia.screensharing.enabled", false);
user_pref("media.getusermedia.audiocapture.enabled", false);
  
// Accept only first party cookies
user_pref("network.cookie.cookieBehavior", 1);
  
// Disable "beacon" asynchronous HTTP transfers (used for analytics)
user_pref("beacon.enabled", false);
  
// Disable leaking network/browser connection type via Javascript (WiFi, cellular, etc.)
user_pref("dom.netinfo.enabled", false);

// https://gist.github.com/chelovekula/c752de4bcbdeef6e4995a6b3467198d4
user_pref("geo.enabled", false);


// The rest I need to review

//        
//        // Disable sensor API
//        user_pref("device.sensors.enabled", false);
//        
//        // Disable battery API
//        user_pref("dom.battery.enabled", false);
//        
//        // Disable gamepad API to prevent USB device enumeration
//        user_pref("dom.gamepad.enabled", false);
//        
//        // Disable virtual reality devices APIs
//        user_pref("dom.vr.enabled", false);
//        
//        // Disable vibrator API
//        user_pref("dom.vibrator.enabled", false);
//        
//        // Disable FlyWeb (discovery of LAN/proximity IoT devices that expose a Web interface)
//        user_pref("dom.flyweb.enabled", false);
//        
//        // Disable webGL
//        user_pref("webgl.disabled", true);
//        
//        // Disable speech recognition
//        user_pref("media.webspeech.recognition.enable", false);
//        
//        // Disable speech synthesis
//        user_pref("media.webspeech.synth.enabled", false);
//        
//        // Disable face detection
//        user_pref("camera.control.face_detection.enabled", false);
//        
//        // Don't monitor OS online/offline connection state
//        user_pref("network.manage-offline-status", false);
//        
//        // Disable video stats to reduce fingerprinting threat
//        user_pref("media.video_stats.enabled", false);
//        
//        // Do not download URLs for the offline cache
//        user_pref("browser.cache.offline.enable", false);
//        
//        // Disable sending Flash Player crash reports
//        user_pref("dom.ipc.plugins.flash.subprocess.crashreporter.enabled",	false);
//        user_pref("dom.ipc.plugins.reportCrashURL", false);
//        
//        // Disable permissions to access the camera, notifications, location, and microphone
//        user_pref("permissions.default.camera", 2);
//        user_pref("permissions.default.desktop-notification", 2);
//        user_pref("permissions.default.geo", 2);
//        user_pref("permissions.default.microphone", 2);
//        
//        
//        /******************************************************************************
//         * Security                                                                   *
//         ******************************************************************************/
//         
//        // Enforce a security delay when installing add-ons (in milliseconds)
//        user_pref("security.dialog_enable_delay", 1000);
//        
//        // Enable Punycode for Internationalized Domain Names to eliminate possible spoofing
//        user_pref("network.IDN_show_punycode", true);
//        
//        // Disable remembering logins
//        user_pref("signon.rememberSignons", false);
//        
//        // Don't trim http:// off of URLs in the address bar
//        user_pref("browser.urlbar.trimURLs", false);
//        
//        // Disable url formatting (different colours for domain and remainer of section)
//        user_pref("browser.urlbar.formatting.enabled", false);
//        
//        // Don't try to guess domain names when entering an invalid domain name in URL bar
//        user_pref("browser.fixup.alternate.enabled", false);
//        user_pref("browser.fixup.hide_user_pass", true);
//        
//        // Enable insecure connection warnings
//        user_pref("security.insecure_connection_text.enabled", true);
//        user_pref("security.insecure_connection_text.pbmode.enabled", true);
//        
//        // Enable insecure password warnings (login forms in non-HTTPS pages)
//        user_pref("security.insecure_password.ui.enabled", true);
//        
//        // Show in-content login form warning UI for insecure login fields
//        user_pref("security.insecure_field_warning.contextual.enabled", true);
//        
//        // disable HTTP2 (which was based on SPDY which is now deprecated)
//        user_pref("network.http.spdy.enabled", false);
//        user_pref("network.http.spdy.enabled.deps", false);
//        user_pref("network.http.spdy.enabled.http2", false);
//        
//        
//        /******************************************************************************
//         * Firefox features and components                                            *
//         ******************************************************************************/
//        
//        // Enable Firefox Tracking Protection
//        user_pref("privacy.trackingprotection.enabled", true);
//        user_pref("privacy.trackingprotection.pbmode.enabled", true);
//        
//        // Enable Safebrowsing (blocking reported web forgeries and attack sites)
//        user_pref("browser.safebrowsing.phishing.enabled", true);
//        user_pref("browser.safebrowsing.malware.enabled", true);
//        
//        // Disable Firefox Account/Sync
//        user_pref("identity.fxaccounts.enabled", false);
//        
//        // Disable Pocket
//        user_pref("extensions.pocket.enabled", false);
//        
//        // Disable Reader
//        user_pref("reader.parse-on-load.enabled", false);
//        
//        // Disable WebIDE
//        user_pref("devtools.webide.enabled", false);
//        user_pref("devtools.webide.autoinstallADBHelper", false);
//        user_pref("devtools.webide.autoinstallFxdtAdapters", false);
//        
//        // Disable remote debugging
//        user_pref("devtools.debugger.remote-enabled", false);
//        user_pref("devtools.chrome.enabled", false);
//        user_pref("devtools.debugger.force-local", true);
//        
//        // Disable querying Google application reputation database for downloaded files
//        user_pref("browser.safebrowsing.downloads.remote.enabled", false);
//        user_pref("browser.safebrowsing.downloads.remote.url", "");
//        
//        // Disable onboarding
//        user_pref("browser.onboarding.enabled", false);
//        user_pref("devtools.onboarding.telemetry.logged", false);
//        
//        // Disable Normandy studies
//        user_pref("app.normandy.enabled", false);
//        user_pref("app.normandy.first_run", false);
//        user_pref("app.normandy.api_url", "");
//        user_pref("app.normandy.user_id", "");
//        
//        // Disable Shield
//        user_pref("app.shield.optoutstudies.enabled", false);
//        user_pref("extensions.shield-recipe-client.enabled", false);
//        user_pref("extensions.shield-recipe-client.user_id", "");
//        user_pref("extensions.shield-recipe-client.api_url", "");
//        
//        // Disable telemetry
//        user_pref("toolkit.telemetry.enabled", false);
//        user_pref("toolkit.telemetry.rejected", true);
//        user_pref("toolkit.telemetry.unified", false);
//        user_pref("toolkit.telemetry.archive.enabled", false);
//        user_pref("toolkit.telemetry.bhrPing.enabled", false);
//        user_pref("toolkit.telemetry.firstShutdownPing.enabled", false);
//        user_pref("toolkit.telemetry.newProfilePing.enabled", false);
//        user_pref("toolkit.telemetry.reportingpolicy.firstRun", false);
//        user_pref("toolkit.telemetry.shutdownPingSender.enabled", false);
//        user_pref("toolkit.telemetry.unifiedIsOptIn", false);
//        user_pref("toolkit.telemetry.updatePing.enabled", false);
//        user_pref("toolkit.telemetry.cachedClientID", "");
//        user_pref("toolkit.telemetry.hybridContent.enabled", false);
//        
//        // Disable ping center telemetry
//        user_pref("browser.ping-centre.telemetry", false);
//        
//        // Disable experiments
//        user_pref("experiments.enabled", false);
//        user_pref("experiments.supported", false);
//        user_pref("experiments.manifest.uri", "");
//        user_pref("network.allow-experiments", false);
//        
//        // Disable sending Firefox crash reports to Mozilla servers
//        user_pref("breakpad.reportURL", "");
//        
//        // Disable Heartbeat (Mozilla user rating telemetry)
//        user_pref("browser.selfsupport.url", "");
//        
//        // Disable sending reports of tab crashes to Mozilla (about:tabcrashed), don't nag user about unsent crash reports
//        user_pref("browser.tabs.crashReporting.sendReport", false);
//        user_pref("browser.crashReports.unsubmittedCheck.enabled", false);
//        
//        // Disable error reporter
//        user_pref("browser.chrome.errorReporter.enabled", false);
//        user_pref("browser.chrome.errorReporter.submitUrl", "");
//        
//        // Disable collecting/sending health report (healthreport.sqlite*)
//        user_pref("datareporting.healthreport.uploadEnabled", false);
//        user_pref("datareporting.healthreport.service.enabled", false);
//        user_pref("datareporting.policy.dataSubmissionEnabled", false);
//        
//        
//        /******************************************************************************
//         * Extensions / Plugins                                                       *
//         ******************************************************************************/
//         
//        // Disable about:addons' Get Add-ons panel (uses Google-Analytics)
//        user_pref("extensions.getAddons.showPane", false); // hidden pref
//        user_pref("extensions.webservice.discoverURL", "");
//        
//        // Disable Screenshots
//        user_pref("extensions.screenshots.disabled", true);
//        user_pref("extensions.screenshots.upload-disabled", true);
//        
//        // Disable scanning for plugins (used to detect flash, java, etc..)
//        user_pref("plugin.scan.plid.all", false);
//        
//        // Disable NPAPI plugins (0 = disabled, 1 = ask, 2 = enabled)
//        user_pref("plugin.state.flash", 0);
//        user_pref("plugin.state.java", 0);
//        
//        // Decrease system information leakage to Mozilla extension blocklist update servers
//        user_pref("extensions.blocklist.url", "https://blocklist.addons.mozilla.org/blocklist/3/%APP_ID%/%APP_VERSION%/");
//        
//        
//        /******************************************************************************
//         * Automatic connections / Pre-fetching                                       *
//         ******************************************************************************/
//        
//        // Disable automatic updates (still autocheck for updates and install manually)
//        user_pref("app.update.auto", false);
//        
//        // Disable automatic search engine updates
//        user_pref("browser.search.update", false);
//        
//        // Disable the background update service
//        user_pref("app.update.service.enabled", false);
//        
//        // Disable background update staging
//        user_pref("app.update.staging.enabled", false);
//        
//        // Disable automatic extension metadata updating (sends daily pings to Mozilla about extensions and recent startups)
//        user_pref("extensions.getAddons.cache.enabled", false);
//        
//        // Disable automatic updating of personas/themes
//        user_pref("lightweightThemes.update.enabled", false);
//        
//        // Disable automatic retrieval of search engine suggestions when typing in the address bar
//        user_pref("browser.search.suggest.enabled", false);
//        
//        // Disable prefetching of <link rel="next"> URLs
//        user_pref("network.prefetch-next", false);
//        
//        // Disable DNS prefetching
//        user_pref("network.dns.disablePrefetch", true);
//        user_pref("network.dns.disablePrefetchFromHTTPS", true); // hidden pref
//        
//        // Disable the predictive service (Necko)
//        user_pref("network.predictor.enabled", false);
//        
//        // Disable captive portal
//        user_pref("network.captive-portal-service.enabled", false);
//        user_pref("captivedetect.canonicalURL", "");
//        
//        // Disable speculative pre-connections (preloading of autocomplete URLs)
//        user_pref("network.http.speculative-parallel-limit", 0);
//        
//        // Disable downloading home page snippets/messages from Mozilla
//        user_pref("browser.aboutHomeSnippets.updateUrl", "");
//        
//        // Disable pinging URIs specified in HTML <a> ping= attributes (but enforce same host just in case)
//        user_pref("browser.send_pings", false);
//        user_pref("browser.send_pings.require_same_host", true);
//        
//        
//        /*******************************************************************************
//         * Newtab functionality                                                        *
//         *******************************************************************************/
//         
//        // Disable new tab page and show a blank tab instead
//        user_pref("browser.newtabpage.enabled", false);
//        user_pref("browser.newtab.url", "about:blank");
//        
//        // Disable ads & preload
//        user_pref("browser.newtabpage.enhanced", false);
//        user_pref("browser.newtab.preload",	false);
//        
//        // Disable activity stream and recommended stories
//        user_pref("browser.newtabpage.activity-stream.enabled", false);
//        user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
//        
//        
//        /*******************************************************************************
//         * Misc tweaks                                                              *
//         *******************************************************************************/
//        
//        // Disable accessibility service
//        user_pref("accessibility.force_disabled", 1);
//        
//        // Disable about:config warning page
//        user_pref("general.warnOnAboutConfig", false);
//        
//        // Disable warning when closing multiple tabs
//        user_pref("browser.tabs.warnOnClose", false);
//        
//        // Disable cosmetic animations
//        user_pref("toolkit.cosmeticAnimations.enabled", false);
//          
//        // Disable UITour backend
//        user_pref("browser.uitour.enabled", false);
//        user_pref("browser.uitour.url", "");
//        
//        // Disable ctrl+tab image previews
//        user_pref("browser.ctrlTab.previews", false);
//        
//        // Disable auto+smooth scrolling
//        user_pref("general.autoScroll", false);
//        user_pref("general.smoothScroll", false);
//        
//        // Disable spell checker
//        user_pref("layout.spellcheckDefault", 0);
//        
//        // Disable copying extra spaces around words when using copy/paste
//        user_pref("layout.word_select.eat_space_to_next_word", false);
//        
//        // Increase the minimum tab width (default: 75)
//        user_pref("browser.tabs.tabMinWidth", 100);
