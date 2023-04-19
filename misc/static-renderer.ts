/*

This file is used as a part of the build process to convert the interlinear JSON translations into HTML.

To run this file:
$ deno run --allow-read --allow-write static-renderer.ts
*/

type InterlinearData = {
    page: number,
    text: {
        km: string[],
        en: (string | string[])[]
    }[]
}[];


function toHTML(x: InterlinearData): string {
    return `
        ${x.map(passage =>
            `<div class="full">
                ${passage.text.map(segment =>
                    `<div class="element">
                        <div class="km">
                            ${segment.km.map(word =>
                                `<a href="https://en.wiktionary.org/wiki/${word}#Khmer">
                                    ${word}
                                </a>`
                            ).join('\n')}
                        </div>
                        <div class="en">
                            ${segment.en.map(word =>
                                (word instanceof Array)
                                    ? `<i>${word.join(' ')}</i>`
                                    : word)
                            .join(' ')}
                        </div>
                    </div>`).join('\n')}
            </div>`).join('')}
      
        <style>
            .full {
                display: flex;
                flex-wrap: wrap;
                column-gap: 10px;
            }
        
            .km {
                font-size: 4rem;
                text-align: center;;
            }
        
            .en {
                font-size: 2rem;
                text-align: center;
            }
        </style>
    `;
}


let contents;
try {
    contents = await Deno.readTextFile('khmer-interlinear-plato.json');
} catch (e) {
    console.log(e.message);
    Deno.exit(0);
}

const data = JSON.parse(contents) as InterlinearData;

const html = toHTML(data);

try {
    await Deno.writeTextFile('plato.html', html);
} catch (e) {
    console.log(e.message);
    Deno.exit(0);
}


