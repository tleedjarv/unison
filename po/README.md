# Translating Unison File Synchronizer

## Translating

You don't need to update the template file (`unison.pot`). To merge any updates
from the template into your language you only need to run (assuming you are in
`po/`):
```
make your_language.po
```

After updating translations, only commit the PO file to source control.

Important! Do not commit any other files other than your language's PO file to
source control.

### Adding a new language

To start translating to a new language, simply copy `unison.pot` to
`your_language.po` and otherwise follow the remaining instructions in this
file.

## Testing translations

To build translations for testing, you need to set `WITH_GETTEXT=true` env
variable, or add it as a `make` argument. See ../INSTALL.md for other possible
values to `WITH_GETTEXT`.

Then run `make -C .. translations` if you are in the `po` directory (otherwise,
skip `-C ..`). For example:
```
make -C .. translations WITH_GETTEXT=true
```
(To limit the languages built, list your languages in `LINGUAS` env variable.)

This will create a directory tree, as follows:
```
├── po
│   ├── LINGUAS
│   ├── Makefile
│   ├── POTFILES
│   ├── README.md
│   ├── lang1.mo
│   ├── lang1.po
│   ├── lang2.mo
│   ├── lang2.po
│   └── unison.pot
└── share
    └── locale
        ├── lang1
        │   └── LC_MESSAGES
        │       └── unison.mo
        └── lang2
            └── LC_MESSAGES
                └── unison.mo
```

If you have built Unison from source in the same source tree and have a
`src/unison` or `src/unison-gui` executable then running that executable will
pick up the newly built translations.

If the `unison` or `unison-gui` executable is located elsewhere then the
easiest way to test translations is to copy the files next to each other to
form a directory tree like this:
```
├── bin
│   ├── unison
│   ├── unison-fsmonitor
│   └── unison-gui
└── share
    └── locale
        └── ...
```
(This can, but does not have to be, a system-wide location like `/usr` or
`/usr/local`.)


Running Unison from a location like this will pick up the translations.
