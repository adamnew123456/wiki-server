import os
import os.path
import queue
import sys
import threading
import urllib.parse as urlparse

from bs4 import BeautifulSoup


def make_printable(text):
    """
    Strips out all non-printable characters from the text, including:

    - 0x00 - 0x20, excluding CR, LF and tab
    - 0x7F
    """
    buffer = []
    for ch in text:
        if ch in '\r\n':
            buffer.append(ch)
        else:
            ord_ch = ord(ch)
            if ord_ch != 0x7F and ord_ch >= 0x20:
                buffer.append(ch)

    return ''.join(buffer)

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
    elif (url == 'http://c2.com/sig/wiki.gif'
          or url == 'http://c2.com/wiki.png'):
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
    Converts a c2 file from the archive dump into a file which can be served at
    the mirror
    """
    doc = None
    src_file = os.path.join(src_dir, filename)
    for encoding in ('utf-8', 'cp1252'):
        try:
            with open(src_file, encoding=encoding) as html_file:
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

    # The wiki's HTML rendering doesn't properly tag paragraphs, instead of
    # wrapping <p> tags around content it'll put an empty paragraph in as a
    # separator:
    #
    # <div id="wiki">
    #  paragraph 1 text text text
    #  <p></p>
    #  paragraph 2 text text text
    #  <p></p>
    #  paragraph 3 text text text
    # </div>
    #
    # We want to take this and reorganize it so that the paragraphs contain
    # the nodes between them:
    #
    # <div id="wiki">
    #  <p>
    #    paragraph 1 text text text
    #  </p>
    #  <p>
    #    paragraph 2 text text text
    #  </p>
    #  <p>
    #    paragraph 3 text text text
    #  </p>
    # </div>
    #
    # Note that for this purpose, we consider <hr> as paragraph-equivalent
    # although we keep it in the text.
    article = doc.select('#wiki')[0]
    article_paragraphs = []
    paragraph_contents = []
    while len(article.contents) > 0:
        child = article.contents[0].extract()
        if isinstance(child, str):
            paragraph_contents.append(child)
        elif child.name == 'p':
            if paragraph_contents:
                child.extend(paragraph_contents)
                article_paragraphs.append(child)
                paragraph_contents = []
        elif child.name == 'hr':
            if paragraph_contents:
                paragraph = doc.new_tag('p')
                paragraph.extend(paragraph_contents)
                article_paragraphs.append(paragraph)
                paragraph_contents = []

            article_paragraphs.append(child)
        else:
            paragraph_contents.append(child)

    if paragraph_contents:
        paragraph = doc.new_tag('p')
        paragraph.extend(paragraph_contents)
        article_paragraphs.append(paragraph)

    article.extend(article_paragraphs)

    # Each page has a 'wiki?' prefix which we want to strip for the clean
    # version
    filename = filename.replace('wiki?', '')
    with open(os.path.join(dest_dir, filename), 'w') as html_file:
        html_file.write(make_printable(str(doc)))


END_OF_STREAM = object()


def worker(work_list, from_dir, to_dir):
    for f in work_list:
        print('Translating', f, '...')
        normalize_file(f, from_dir, to_dir)


if __name__ == '__main__':
    threads = 6
    work_lists = [[] for i in range(threads)]

    i = 0
    for file in os.listdir(sys.argv[1]):
        if os.path.isdir(os.path.join(sys.argv[1], file)):
            continue
        work_lists[i].append(file)
        i = (i + 1) % threads

    workers = [threading.Thread(target=worker,
                                args=(work_list, sys.argv[1], sys.argv[2]))
               for work_list in work_lists]

    for worker in workers:
        worker.start()

    for worker in workers:
        worker.join()
