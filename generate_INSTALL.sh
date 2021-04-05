#! /bin/sh

# This script generates a plain text files from the source install.rst
# without employing any fancy reStructuredText grammar.
# Of course, this means the markup used in install.rst has to be simple.

# In fact, we only treat
# - certain macros of the form |...|,
# - remove the underlining of subsubsections,
# - remove the underlining of subsubsubsections,
# - replace double `` by a single doublequote,
# - remove "::" at the end of a line,
# - remove the trailing underscore that directly follows a word.

# Additionally, we add references to the source file install.rst together
# with a weblink to its rendered form on the FriCAS homepage
# and we add links for the |...| macros at the bottom of the file.


cat <<'EOF' > INSTALL
# This file is generated via "sh generate_INSTALL.sh" from
# "src/doc/sphinx/source/install.rst".
# See rendered form at https://fricas.github.io/install.html.

EOF

sed -n \
    -e '/^.. contents/d' \
    -e '/^   :local:/d' \
    -e '/^   :depth:/d' \
    -e '/^[\^][\^]*$/d' \
    -e '/^""*$/d' \
    -e '/^ *::$/d' \
    -e 's/::$//' \
    -e 's/|git repository|/FriCAS git repository/g' \
    -e 's/|home page|/FriCAS home page/g' \
    -e 's/|PACKAGE_BOOK|/FriCAS Book/g' \
    -e 's/\([A-Z][a-zA-Z]*\)_\([^A-Z]\)/\1\2/g' \
    -e 's/`\([A-Z][-. :<>/a-zA-Z]*\)`_/\1/g' \
    -e 's/``/"/g' \
    -e 's/^.. _//' \
    -e 'p' \
    src/doc/sphinx/source/install.rst >> INSTALL

# Add links for Sphinx macros at the bottom of the file.
cat <<'EOF' >> INSTALL
FriCAS git repository: https://github.com/fricas/fricas
FriCAS home page: https://fricas.github.io
FriCAS Book: https://fricas.github.io/book.pdf
EOF
