// Live UI check. Uses the existing education and butterflies snapshots.
const { chromium } = require('playwright');
const assert = require('node:assert/strict');
(async () => {
  const browser = await chromium.launch({headless:true,
    executablePath:process.env.CHROMIUM,args:['--no-sandbox']});
  try {
    const page = await browser.newPage({viewport:{width:1440,height:1050}});
    const errors = [];
    page.on('pageerror', e => errors.push(e.message));
    await page.goto((process.env.TAPOSAUR_URL || 'http://localhost:8766') + '/review-snapshot?id=1');
    const table = page.locator('table[data-sortable]').first();
    const title = table.locator('tbody tr').filter({has:page.getByText('Title',{exact:true})});
    assert.match(await title.locator('[role=img]').getAttribute('aria-label'), /43.3%/);
    await title.locator('td').nth(1).getByRole('link',{name:'13',exact:true}).click();
    await page.waitForURL('**/review-papers?**');
    assert.equal(await page.locator('tbody tr').count(),13);
    assert.match(await page.locator('main').innerText(), /13 matching papers/);
    assert.equal(await page.getByText('Title unavailable',{exact:true}).count(),0);
    await page.getByRole('link',{name:'← Snapshot coverage'}).click();
    const header = table.getByRole('button',{name:'Missing',exact:false});
    await header.click();
    const values = () => table.locator('tbody tr').evaluateAll(rows =>
      rows.map(r => Number(r.cells[2].dataset.sortValue)));
    let v = await values();
    assert.deepEqual(v,[...v].sort((a,b)=>b-a));
    await header.focus();
    await page.keyboard.press('Enter');
    v = await values();
    assert.deepEqual(v,[...v].sort((a,b)=>a-b));
    assert.equal(await header.locator('..').getAttribute('aria-sort'),'ascending');
    await page.goto((process.env.TAPOSAUR_URL || 'http://localhost:8766') + '/review-snapshot?id=3');
    const publishers = page.locator('table[data-sortable]').nth(1);
    const gap = publishers.getByRole('button',{name:'Missing abstract',exact:false});
    await gap.click();
    await page.locator('[data-sort-basis]').selectOption('ratio');
    const ratios = await publishers.locator('tbody tr').evaluateAll(rows =>
      rows.map(r => Number(r.cells[3].dataset.sortRatio)));
    assert.deepEqual(ratios,[...ratios].sort((a,b)=>b-a));
    const first = publishers.locator('tbody tr').first();
    const expected = Number(await first.locator('td').nth(3).getAttribute('data-sort-value'));
    await page.screenshot({path:'/tmp/taposaur-snapshot-dashboard.png',fullPage:false});
    await first.locator('td').nth(3).getByRole('link').click();
    await page.waitForURL('**/review-papers?**');
    assert.match(await page.locator('main').innerText(),new RegExp(expected.toLocaleString('en-US')+' matching papers'));
    assert.equal(await page.locator('tbody tr').count(),Math.min(50,expected));
    const firstDois = await page.locator('tbody tr code').allTextContents();
    if(expected>50) {
      await page.getByRole('link',{name:'Next 50 papers →'}).click();
      const next = await page.locator('tbody tr code').allTextContents();
      assert(next.every(doi=>!firstDois.includes(doi)));
      assert(page.url().includes('publisher='));
      assert(page.url().includes('availability=missing'));
    }
    await page.setViewportSize({width:390,height:844});
    await page.goto((process.env.TAPOSAUR_URL || 'http://localhost:8766') + '/review-snapshot?id=1');
    assert(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));
    assert.deepEqual(errors,[]);
    console.log('Count links, exact membership, numeric/percentage sort, keyboard, pagination and mobile: passed');
  } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exitCode=1});
