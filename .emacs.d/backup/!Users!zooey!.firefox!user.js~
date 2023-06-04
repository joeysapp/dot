/* https://github.com/arkenfox/user.js/wiki/2.1-User.js */
/* look @ ./prefs.js */

user_pref("gfx.font_rendering.opentype_svg.enabled", false);

/* 1402: limit font visibility (Windows, Mac, some Linux) [FF94+]
 * Uses hardcoded lists with two parts: kBaseFonts + kLangPackFonts [1], bundled fonts are auto-allowed
 * In normal windows: uses the first applicable: RFP (4506) over TP over Standard
 * In Private Browsing windows: uses the most restrictive between normal and private
 * 1=only base system fonts, 2=also fonts from optional language packs, 3=also user-installed fonts
 * [1] https://searchfox.org/mozilla-central/search?path=StandardFonts*.inc ***/
   // user_pref("layout.css.font-visibility.private", 1);
   // user_pref("layout.css.font-visibility.standard", 1);
   // user_pref("layout.css.font-visibility.trackingprotection", 1);
user_pref("layout.css.font-visibility.private", 1);

/* 2601: prevent accessibility services from accessing your browser [RESTART]
 * [1] https://support.mozilla.org/kb/accessibility-services ***/
user_pref("accessibility.force_disabled", 1);
