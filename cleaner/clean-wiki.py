import os
import os.path
import sys
import urllib.parse as urlparse

from bs4 import BeautifulSoup

#
# The structure we're looking for is something like this:
#
#    - Single Page:  /wiki/:page:
#    - Title Search: /wiki/
#    - Full Search:  /fullSearch
#    - Logo:         /static/wiki.gif

def map_url(url, page):
    """
    Converts a c2 Wiki URL into a relative URL.

    This assumes that the current page is somewhere in the wiki/ path, so all
    links to site-wide pages are up one level
    """
    if url == 'http://c2.com/cgi/fullSearch':
        return '../fullSearch?search=' + urlparse.quote(page)
    elif url == 'http://c2.com/sig/wiki.gif' or url == 'http://c2.com/wiki.png':
        return '../static/wiki.gif'
    elif url == 'http://c2.com/cgi/wiki':
        return '../wiki'
    elif url.startswith('wiki%3F'):
        # Also get rid of the trailing .html
        return url[7:-5]
    elif url.startswith('http://c2.com/cgi/wiki?edit='):
        return url[28:]
    elif url.startswith('http://c2.com/cgi/quickDiff?'):
        return url[28:]
    elif url.startswith('http://c2.com/cgi/wiki?'):
        return url[23:]
    elif url.startswith('http://c2.com/cgi/fullSearch'):
        return '../fullSearch' + url[28:]
    else:
        return url

def normalize_file(filename, src_dir, dest_dir):
    """
    Converts a c2 file from the archive dump into a file which can be served at the
    mirror
    """
    doc = None
    for encoding in ('utf-8', 'cp1252'):
        try:
            with open(os.path.join(src_dir, filename), encoding=encoding) as html_file:
                doc = BeautifulSoup(html_file.read(), 'lxml')

            break
        except UnicodeDecodeError:
            pass

    if doc is None:
        return

    # The scripts included aren't required for the page to function, mostly
    # Google Analytics stuff. All the dynamism we care about is handled server
    # side and controlled in the browser via forms.
    for script in doc('script'):
        script.extract()

    page_name = (os.path.basename(filename)
                 .replace('wiki?', '')
                 .replace('.html', ''))
    for tag in doc.find_all():
        if 'src' in tag.attrs:
            tag.attrs['src'] = map_url(tag.attrs['src'], page_name)

        if 'href' in tag.attrs:
            tag.attrs['href'] = map_url(tag.attrs['href'], page_name)

        # Forms
        if 'action' in tag.attrs:
            tag.attrs['action'] = map_url(tag.attrs['action'], page_name)

    # Each page has a 'wiki?' prefix which we want to strip for the clean version
    filename = filename.replace('wiki?', '')
    with open(os.path.join(dest_dir, filename), 'w') as html_file:
        html_file.write(str(doc))

if __name__ == '__main__':
    for file in os.listdir(sys.argv[1]):
        print('Translating', file, '...')
        normalize_file(file, sys.argv[1], sys.argv[2])
