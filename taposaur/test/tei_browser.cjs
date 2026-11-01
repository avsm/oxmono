const { chromium } = require('playwright');
const fs = require('fs');
(async () => {
 const browser = await chromium.launch({headless:true,executablePath:process.env.CHROMIUM,args:['--no-sandbox']});
 try {
 const page = await browser.newPage({viewport:{width:1450,height:1000}});
 const errors=[];
 page.on('pageerror',e=>errors.push(e.message));
 await page.goto((process.env.TAPOSAUR_URL || 'http://localhost:8766') + '/paper?uid=doi%3A10.0000%2Fanziamj.v56i0.9365#tei=tei.xml');
 await page.waitForSelector('#panel-body .tei-reference',{timeout:60000});
 console.log('references',await page.locator('.tei-reference').count());
 console.log('DOI links',await page.locator('.tei-reference a[href^="https://doi.org/"]').count());
 console.log('journal links',await page.locator('.tei-reference a[href^="https://openalex.org/"]').count());
 if(await page.locator('.tei-reference a[href^="https://openalex.org/"]').count()===0)
   throw Error('Verified OpenAlex links are missing');
 if(await page.locator('.tei-reference a[href^="https://doi.org/"]').first().getAttribute('target')!=='_blank')
   throw Error('DOI link would replace the reader');
 const citation=page.locator('#panel-body a[href="#tei-623335"]').first();
 await citation.click();
 if(await page.locator('#paper-panel').isHidden()) throw Error('Citation closed sidebar');
 if(!page.url().endsWith('#tei=tei.xml')) throw Error('Citation changed panel route');
 const raw=await page.locator('#tei-623335 .tei-reference-raw').innerText();
 if(!raw.includes('1992 Université')) throw Error('Raw reference spacing not fixed');
 await page.screenshot({path:'/tmp/taposaur-references.png',fullPage:false});
 await page.locator('#tei-623335 .tei-backlinks a').first().click();
 if(await page.locator('#paper-panel').isHidden()) throw Error('Backlink closed sidebar');
 await page.goto((process.env.TAPOSAUR_URL || 'http://localhost:8766') + '/read?uid=doi%3A10.0000%2Fanziamj.v56i0.9365&file=tei.xml');
 await page.waitForSelector('.tei-reference');
 await page.locator('a[href="#tei-623335"]').first().click();
 if(!page.url().endsWith('#tei-623335')) throw Error('Standalone anchor navigation failed');
 // Exercise the exact server-rendered scholarly fixture with the real client.
 await page.setContent('<!doctype html>' + fs.readFileSync('/tmp/taposaur-structured-tei.html','utf8'));
 await page.addScriptTag({url:(process.env.TAPOSAUR_URL || 'http://localhost:8766') + '/assets/katex.js'});
 await page.addScriptTag({url:(process.env.TAPOSAUR_URL || 'http://localhost:8766') + '/assets/interface.js'});
 if(await page.locator('[data-tex] math').count()!==1) throw Error('TeX failed to render');
 if(await page.locator('math mfrac').count()!==1) throw Error('MathML fraction missing');
 if(await page.locator('th[colspan="2"]').count()!==1) throw Error('Table header lost');
 await page.route('https://example.org/a.png',route=>route.fulfill({
   contentType:'image/png',body:Buffer.from(
     'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+jRz8AAAAASUVORK5CYII=',
     'base64')}));
 if(await page.locator('.tei img').count()!==0) throw Error('Image loaded before activation');
 await page.locator('.tei-image-link').click();
 await page.waitForFunction(()=>document.querySelector('.tei img')?.naturalWidth===1);
 console.log('math, tables, sidebar, backlinks: passed');
 if(errors.length) throw Error(errors.join('\n'));
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1)});
