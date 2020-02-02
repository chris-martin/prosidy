title:     Prosidy
subtitle:  A minimal language for writing.
toc-title: Home
created:   2020-01-05
updated:   2020-02-02
priority:  10
lang:      en
---

Prosidy is a small language for writing documents in plain text.

Like #link[url='https://daringfireball.net/projects/markdown/']{Markdown},
Prosidy is an "easy-to-read, easy to write" plain-text format.
It doesn't get in the way of your text so the source document is legible,
and has only a few rules to keep in mind when writing.

Like #link[url='https://www.w3.org/XML/']{XML}, Prosidy is #i{extensible}:
it doesn't have any preconceived ideas about the what you write,
letting the author tailor the language presicely to their use case.


#-section[title='Syntax sample']:
  #=src[lang='prosidy']:endsrc
recipe: A simple roux
---

#-ingredients:
  #-item[amount='1/8', unit='cup']{flour}
  #-item[amount='1/8', unit='cup']{butter}
#:

#-implements:
  #-item{fry pan}
  #-item{wooden spoon}
#:

#-step:
  Add the #item{butter} to the #item{fry pan}.
  Melt over #heat{medium-low} heat.
#:

#-step:
  Add the flour to the melted butter.
  Stir constantly in a figure-eight motion.
#:

#-step:
  Stop stiring when the roux is the desired color.
  The darker brown the roux is, the stronger the flavor of the dish.
#:
  #:endsrc
#:

#-section[title: 'Get Prosidy']:
  #-note[level: 'important']:
    Prosidy is currently #i{pre-alpha} software.

    While it's more than usable 
    (#link[url='https://git.fldcr.com/prosidy/tree/doc/index.pro']{this site is built with it}),
    neither its specification nor the reference tooling have stabalized. 
    
    Use at your own risk.
  #:


  Prosidy is available under the
  #link[url='https://www.mozilla.org/en-US/MPL/2.0/']{MPL v2.0}
  license.
  The latest source can be downloaded as an archive in
  #link[url='https://git.fldcr.com/prosidy/snapshot/prosidy-master.tar.gz']{#lit{.tar.gz}}
  or
  #link[url='https://git.fldcr.com/prosidy/snapshot/prosidy-master.zip']{#lit{.zip}}
  format.

  Alternatively, you can clone it via Git at
  #link[url='https://git.fldcr.com/prosidy']{https://git.fldcr.com/prosidy}.
#: