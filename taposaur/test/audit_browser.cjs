const {chromium}=require('playwright');
const assert=require('node:assert/strict');
const base=process.env.TAPOSAUR_URL || 'http://localhost:8766';
(async()=>{
  const browser=await chromium.launch({executablePath:process.env.CHROMIUM,
    headless:true,args:['--no-sandbox']});
  try {
    const page=await browser.newPage({viewport:{width:1280,height:900}});
    const errors=[];page.on('pageerror',e=>errors.push(e.message));
    await page.goto(base+'/audit?actor=system');
    assert(await page.locator('tbody tr').count()>0);
    await page.locator('tbody tr details').first().locator('summary').click();
    const run=page.locator('tbody tr').first().locator('a[href^="/audit?run="]');
    await run.click();
    await page.waitForURL('**/audit?run=*');
    const runId=new URL(page.url()).searchParams.get('run');
    const json=await (await page.request.get(base+'/audit?format=jsonl&run='+runId)).text();
    const events=json.trim().split('\n').map(JSON.parse);
    assert(events.every(e=>e.run===runId && e.actor==='system'));
    const operation=events[0].operation;
    const pair=await (await page.request.get(base+'/audit?format=jsonl&operation='+operation)).text();
    assert(pair.trim().split('\n').map(JSON.parse).every(e=>e.operation===operation));
    await page.screenshot({path:'/tmp/taposaur-audit.png'});
    await page.getByRole('link',{name:'Incomplete operations',exact:true}).click();
    await page.waitForURL('**/audit?incomplete=1');
    await page.setViewportSize({width:390,height:844});
    await page.goto(base+'/audit');
    assert(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));
    await page.goto(base+'/debug');
    assert((await page.locator('main').innerText()).includes('Audit log (--audit-db)'));
    assert.deepEqual(errors,[]);
    console.log('Audit UI: filters, run/operation queries, JSONL, incomplete view, storage path and mobile passed.');
  }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1});
