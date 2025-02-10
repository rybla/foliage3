const serve_dir = "docs"
const port = 8000;

Bun.serve({
  port,
  async fetch(req) {
    const url_str = req.url.endsWith("/") ? `${req.url}index.html` : req.url
    const url = new URL(url_str);
    const filePath = `${serve_dir}${url.pathname}`;
    console.log(`GET ${filePath}`)
    const file = Bun.file(filePath);
    if (!(await file.exists())) return new Response(`Not Found: ${url_str}`, { status: 404 });
    return new Response(file);
  }
});

console.log(`serving at http://localhost:${port}`)