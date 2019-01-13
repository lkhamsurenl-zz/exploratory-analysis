import os
from os import walk

PWD = os.path.dirname(os.path.realpath(__file__))

def main():
    paths = []
    for (dirpath, dirnames, filenames) in walk(PWD):
        for filename in filenames:
            name_chunks = filename.split('.')
            if (
                not name_chunks or
                name_chunks[-1] != 'html' or
                'Rproj' in dirpath or
                (len(name_chunks) > 2 and name_chunks[-2] == 'nb')
                ):
                continue
            paths.append(os.path.join(dirpath, filename))

    # Sort them
    paths.sort()

    content = """
        <html>
            <ul>
                {}
            </ul>
        <html>
    """.format('\n'.join(
        ['<li><a href="{}">{}</a></li>'.format(path.replace(PWD, '.'), path.replace(PWD, '')) for path in paths]
    ))

    with open(os.path.join(PWD, 'index.html'), 'wb') as f:
        try:
            f.write(content)
        except Exception, e:
            logging.error("Error in writing output: {e}".format(**locals()))

if __name__ == "__main__":
    main()
