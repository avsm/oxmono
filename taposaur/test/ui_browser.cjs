const { chromium } = require('playwright');
const assert = require('node:assert/strict');
const base = process.env.TAPOSAUR_URL || 'http://localhost:8766';
(async () => {
  const browser = await chromium.launch({headless:true,
    executablePath:process.env.CHROMIUM,args:['--no-sandbox']});
  try {
    const page = await browser.newPage({viewport:{width:1280,height:900}});
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(base + '/reviews');
    const sort = page.getByRole('button', {name:'Review',exact:false}).first();
    await sort.click();
    assert.equal(await sort.locator('..').getAttribute('aria-sort'),'ascending');
    await page.goto(base + '/search?journal=Agronomy%20Journal&metadata=abstract');
    const toggle = page.getByRole('checkbox',{name:'Show abstracts'});
    assert(await page.locator('.abstract-snippet').count() > 0);
    await toggle.focus();
    await page.keyboard.press('Space');
    assert.equal(await page.locator('.abstract-snippet').first().isVisible(),false);
    await page.reload();
    assert.equal(await toggle.isChecked(),false);
    await toggle.check();
    assert.equal(await page.locator('.abstract-snippet').first().isVisible(),true);
    await page.screenshot({path:'/tmp/taposaur-search-after.png'});
    await page.getByRole('combobox',{name:'Journal',exact:true}).fill('Agronomy');
    await page.locator('#journal-options [role=option]').first().waitFor();
    await page.keyboard.press('Escape');
    assert.equal(await page.locator('#journal-menu').isVisible(),false);
    await page.goto(base + '/paper?uid=doi%3A10.1002%2Fagj2.21016');
    const files = page.locator('table').filter({has:page.getByRole('button',{name:'Size',exact:false})});
    await files.getByRole('button',{name:'Size',exact:false}).click();
    const values = await files.locator('tbody tr').evaluateAll(rows =>
      rows.map(row => Number(row.cells[4].dataset.sortValue)));
    assert.deepEqual(values,[...values].sort((a,b)=>b-a));
    await page.screenshot({path:'/tmp/taposaur-paper-after.png'});
    for (const route of ['/reviews','/search?journal=Agronomy%20Journal',
      '/review-snapshot?id=2','/paper?uid=doi%3A10.1002%2Fagj2.21016']) {
      await page.setViewportSize({width:390,height:844});
      await page.goto(base + route);
      assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth),route);
    }
    assert.deepEqual(errors,[]);
    console.log('Shared UI: sorting, checkbox persistence, autocomplete and mobile layout passed.');
  } finally { await browser.close(); }
})().catch(error => {console.error(error);process.exitCode=1;});
