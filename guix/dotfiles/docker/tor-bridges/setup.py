import sys
from setuptools import setup

MAJOR_VERSION = '0'
MINOR_VERSION = '0'
MICRO_VERSION = '1'
VERSION = "{}.{}.{}".format(MAJOR_VERSION, MINOR_VERSION, MICRO_VERSION)

setup(name='tor-bridges',
      version=VERSION,
      description="Get Tor bridges via email and serve them via HTTP service",
      url='https://github.com/kitnil/tor-bridges',
      author='Oleg Pykhalov',
      author_email='go.wigust@gmail.com',
      license='GPLv3',
      packages=['tor_bridges'],
      classifiers=[
          'Environment :: Console',
          'Intended Audience :: End Users/Desktop',
          'Intended Audience :: Developers',
          'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
          'Operating System :: MacOS :: MacOS X',
          'Operating System :: Unix',
          'Operating System :: POSIX',
          'Programming Language :: Python',
          'Programming Language :: Python :: 3.6',
          'Development Status :: 5 - Production/Stable',
          'Topic :: Office/Business'
      ],
      zip_safe=False,
      entry_points={'console_scripts': ['tor-bridges = tor_bridges.__main__:main']},
      platforms='any')
