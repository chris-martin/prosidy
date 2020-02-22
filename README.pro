title: Prosidy README
---

Prosidy is a small language for writing documents.

Like #link[url='https://daringfireball.net/projects/markdown/']{Markdown},
Prosidy's syntax is lightweight; it doesn't get in the way of your text.

Like #link[url='https://www.w3.org/XML/']{XML},
Prosidy is extensible: it doesn't make any assumptions about your content.
You'll never have to fight to make your data fit a structure that wasn't designed for it.

#=src[lang='prosidy']:end
recipe: A simple roux
---
In a #ware{medium saucepan},
heat #ingredient[amount='25', unit='g']{oil}
until dropping a pinch of flour into the oil causes it to bubble.

#ware{Whisk} #ingredient[amount='25', unit='g']{flour}
into the oil until the roux is the desired color.
#:end

All of the code here is under #b{heavy} development;
be careful before using it for anything critical!

That said, feedback is more than welcome!
Reach me at #link[url='mailto:alex@fldcr.com']{alex@fldcr.com}.

#-h{Developing Prosidy}
This project sets up an environment using direnv and Nix.
We'd recommend you have both installed before building,
although it may work without both.

#-h{Changelog}
#-h+{2020-02-22}
#-list:
  #-item{Large refactor to allow unopinionated use of this library. Optics are no longer "basically required".}
  #-item{Full documentation of the public-facing API.}
#:

#-h+{2020-02-15}
#-list:
  #-item{Pulled apart the monorepo again to allow for uploading to Hackage.}
#:

#-h+{2020-01-28}
#-list:
  #-item{Fixed a few parser errors, specifically around comments.}
  #-item{Added support for the first line of a Prosidy file to be a hash-bang (in both VSCode and the core parser)}
#:

#-h+{2020-01-24}
#-list:
  #-item{Combined multiple Cabal projects into a super-Cabal file.}
#:

#-h+{2020-01-19}
#-list:
  #-item{Added source tags to all Prosidy elements meaning better errors!}
#:

#-h+{2020-01-03}
#-list:
  #-item{Refactored the various Prosidy repositories into a single monorepo.}
#:
