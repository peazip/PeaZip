Notes to 8.2.0 release

Pre-existing text string modified:
txt_7_1_new: Extract all here (in new folder)
txt_7_1_smart: Extract all here (smart new folder)


= GENERAL NOTES =

This package, named peazip-x.y.about_translations.zip, contains data and information for translators. 
It can be found in "PeaZip translations" download page, and also on other repositories of PeaZip project.

= HOW TO TRANSLATE THE APPLICATION =

Language files are contained in "lang" folder. 
Instead, if you are interested in translating Windows context menu entries (for W7 and above) please refer to instructions in "readme_about_windows_context_menu.txt".

Language files are plain UTF-8 with BOM encoded text files which can be freely edited using any suitable text editor.
To create a new translation file you can:
1 - make a copy of default.txt (in PeaZip's path in /res/lang subfolder) or of any other language file if you prefer starting from a language other than English;
2 - edit lines 2 to 6 of the document to enter language name (both in English and in the original language for better readability), PeaZip's version (major.minor) the translation is aimed to, translator's and last last revisor's name and last revision date;
3 - translate the text after the "variable_name: " part (don't add, move or delete lines, don't change the "variable_name: " part);
4 - optionally, translate the mini-tutorial after "=== about text group ===" line (free editing, it is loaded and displayed "as is" as application's mini-tutorial); it is very important to improve usability of the program for non-English speaking users.
5 - name your language file: it is recommended to use the standard two letter international code to identify the country https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 

As alternative tool to help translators, you can find "translations.xls" to help in creating and maintaining localizations, simply compiling column D.
The spreadsheet contains 2 pages to be compiled: "PeaZip text group", and "PeaLauncher text group".
The pages need to be completed and pasted (column E) in the language file; the "About text group" can be freely edited in the language file itself.
The spreadsheet shows variable name (column B), corresponding text string in english (column C) and a blank, yellow column (D) for typing the translated text strings.
On the right, a column E (blue) will show the "variable_name: " part assembled with the translated string: the content of this area can be copied and paste to replace the text in "=== PeaZip text group ===" and "=== PeaLauncher text group ===" sections (the spreadsheet features TWO pages, one for each of the two groups).
Lines must be pasted in the original order (it is sufficient to sort them by column F).
USE OF THER SPREADSHEET IS OPTIONAL, you can directly modify any existing language file if you prefer.

= SUGGESTIONS =

At each version all language files are mass-updated, with missing text lines in English; to update a localization, it's enough to update the English text lines.
For a better result it is also recommended to check all the language file using them in the application, to see if the update is coherent with linguistic style used by the translator of the current version.
For languages spoken in different ways in different countries (i.e. English, Spanish, Portuguese...) it is recommended to fork the translation, creating i.e. en-us, pt-br etc

= HOW TO CONTRIBUTE TRANSLATIONS =

MAIL
Language files can be sent to giorgio.tani.software@gmail.com, to be evaluated for inclusion in future updates or publication in Translations repository
https://github.com/peazip/PeaZip-Translations

GIT
Language files can be updated on Git repository so commits can be evaluated and merged in the online code base
https://github.com/peazip/PeaZip/tree/welcome/peazip-sources/res/lang (language files)
https://github.com/peazip/PeaZip/tree/welcome/peazip-sources/res/lang-wincontext (W7+ context menu items)

All translated language files should be considered as released under GFDL, GNU Free Documentation License, as they have to be considered derivate work from the application's language file which is released under GFDL.