SVN::Web provides a web interface to subversion repositories. Features include:

  * Viewing multiple Subversion repositories.  SVN::Web is a full Subversion client, so you can access repositories on the local disk (with the file:/// scheme) or that are remotely accessible using the http:// and svn:// schemes.

  * Browsing every revision of the repository.

  * Viewing the contents of files in the repository at any revision.

  * Viewing diffs of arbitrary revisions of any file.  Diffs can be viewed as plain unified diffs, or HTML diffs that use colour to more easily show what's changed.

  * Viewing the revision log of files and directories, see what was changed when, by who.

  * Viewing the blame/annotation details of any file.

  * Generating RSS feeds of commits, down to the granularity of individual files.  The RSS feeds are auto-discoverable in modern web browsers.

  * Viewing everything that was changed in a revision, and step through revisions one at a time, viewing the history of the repository.

  * Viewing the interface in a number of different languages.  SVN::Web's interface is fully templated and localised, allowing you to change the look-and-feel without writing any code; all strings in the interface are stored in a separate file, to make localising to different languages easier.

  * Rich log message linking.  You can configure SVN::Web to recognise patterns in your log messages and automatically generate links to other web based systems.  For example, if your log messages often refer to tickets in your request tracking system "Reported in: t#1234" then SVN::Web can turn t#1234 in to a link to that ticket.  SVN::Web can also be configured to recognise e-mail addresses, URLs, and anything else you wish to make clickable.

  * Caching.  Internally, SVN::Web caches most of the data it gets from the repository, helping to speed up repeated visits to the same page, and reducing the impact on your repository server.

  * As SVK repositories are also Subversion repositories, you can do all of the above with those too.

Additional actions can easily be added to the base set supported by the core of SVN::Web.