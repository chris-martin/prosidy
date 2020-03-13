title: Prosidy README
---

#link[uri='https://ci.fldcr.com/prosidy/prosidy']{
#image[uri='https://ci.fldcr.com/api/badges/prosidy/prosidy/status.svg', desc='Build Status']
}
#link[uri='https://hackage.haskell.org/packages/prosidy']{
#image[uri='https://img.shields.io/hackage/v/prosidy', desc='Hackage']
}

Prosidy is a small language for writing documents.

Like #link[uri='https://daringfireball.net/projects/markdown/']{Markdown},
Prosidy's syntax is lightweight; it doesn't get in the way of your text.

Like #link[uri='https://www.w3.org/XML/']{XML},
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
Reach me at #link[uri='mailto:alex@fldcr.com']{alex@fldcr.com}.


#-h{Related projects}

#-h+{VSCode plugin}
#-list:
  #-item:
    homepage:
    #link[uri='https://marketplace.visualstudio.com/items?itemName=prosidy.prosidy']{
        marketplace.visualstudio.com/items?itemName=prosidy.prosidy
    }
  #:
  #-item:
    source: 
    #link[uri='https://git.fldcr.com/prosidy/vscode']{
        git.fldcr.com/prosidy/vscode
    }
  #:
#:
Official VSCode support for the Prosidy language.


#-h+{#lit{prosidyc}}
#-list:
  #-item:
    homepage:
    #link[uri='https://hackage.haskell.org/packages/prosidyc']{
        hackage.haskell.org/packages/prosidyc
    }
  #:
  #-item:
    source: 
    #link[uri='https://git.fldcr.com/prosidy/prosidyc']{
        git.fldcr.com/prosidy/prosidyc
    }
  #:
#:
The #lit{prosidyc} Haskell library provides a small DSL for compiling
Prosidy documents into other formats, ensuring that source locations are
attached to error messages.
