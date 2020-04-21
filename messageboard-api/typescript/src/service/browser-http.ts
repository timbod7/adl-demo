/** HTTP communications using built in browser fetch */

import { HttpFetch, HttpRequest, HttpResponse } from "./http";

export class BrowserHttp implements HttpFetch {
  async fetch(request: HttpRequest): Promise<HttpResponse> {
    const resp = await fetch(request.url, {
      method: request.method,
      headers: request.headers,
      body: request.body,
    });

    return {
      status: resp.status,
      statusText: resp.statusText,
      ok: resp.ok,
      json: () => resp.json(),
      text: () => resp.text(),
    };
  }
}
