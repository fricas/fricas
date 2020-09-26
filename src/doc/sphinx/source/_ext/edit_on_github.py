"""
Sphinx extension to add ReadTheDocs-style "Edit on GitHub" links to the
sidebar.

Loosely based on https://github.com/astropy/astropy/pull/347
"""

import os


__licence__ = 'BSD (3 clause)'


def html_page_context(app, pagename, templatename, context, doctree):
    if templatename != 'page.html':
        return

    # Do not show "edit on github" for generated api documentation.
    if pagename.split('/')[0] == 'api':
        return

    path = os.path.relpath(doctree.get('source'), app.builder.srcdir)
    cfg = app.config
    context['show_on_github_url'] = cfg.show_on_github_url.format(path=path)
    context['edit_on_github_url'] = cfg.edit_on_github_url.format(path=path)


def setup(app):
    app.add_config_value('show_on_github_url', 'https://github.com', True)
    app.add_config_value('edit_on_github_url', 'https://github.com', True)
    app.connect('html-page-context', html_page_context)
