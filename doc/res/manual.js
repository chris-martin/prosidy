class CalculateViewing {
    constructor() {
        this.main = document.querySelector('main');
        this.currentLinks = document.querySelectorAll('nav > ol > li.current a')
        this.parents = document.querySelectorAll('nav > ol > li, nav > ol > li > ol > li')
        this.sections = document.querySelectorAll('main > section, main > section > section')
        this.update();
        window.currentLinks = this.currentLinks
    }


    update() {
        if (!this.currentLinks) return;
        const currentTop = window.scrollY;
        let viewing;
        for (const section of this.sections) {
            if (section.offsetTop > currentTop) break;
            viewing = section.id;
        }
        for (const link of this.currentLinks) {
            if (viewing === link.dataset.target) {
                for (const li of this.parents) {
                    if (li.contains(link)) {
                        li.classList.add('viewing-child');
                    } else {
                        li.classList.remove('viewing-child');
                    }
                }
                link.classList.add('viewing');
            } else {
                link.classList.remove('viewing');
            }
        }
    }
}

const updateSourceBlocks = () => {
    for (const snippet of document.querySelectorAll('pre.source-code')) {
        const lines = snippet.querySelectorAll(':scope > code > code.source-line');
        const {min, max} = Array.from(lines).reduce((acc, line) => {
            const {min, max} = acc;
            const lineNumber = Number.parseInt(line.dataset.lineNumber, 10);
            return {
                min: min && min < lineNumber ? min : lineNumber,
                max: max && max > lineNumber ? max : lineNumber,
            }
        }, {});

        if (min && max) {
            const el = document.createElement('div');
            el.classList.add('line-numbers');
            for (let nth = min; nth <= max; nth ++) {
                const number = document.createElement('code');
                number.classList.add('line-number');
                number.innerText = nth;
                el.append(number);
            }
            snippet.prepend(el);
        }

        const language = snippet.dataset.language;
        if (language) {
            const lang = document.createElement('div');
            lang.classList.add('language');
            lang.innerText = language;
            snippet.prepend(lang);
        }
    }
};

const main = () => {
    updateSourceBlocks();
    const calc = new CalculateViewing;
    const update = () => calc.update();
    window.addEventListener('scroll', update);
    window.addEventListener('resize', update);
}

window.addEventListener('load', main);