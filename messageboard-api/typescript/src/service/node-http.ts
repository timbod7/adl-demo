/** HTTP communications using node-fetch */

import nodefetch from "node-fetch";

import { HttpFetch, HttpRequest, HttpResponse } from "./http";

export class NodeHttp implements HttpFetch {
  async fetch(request: HttpRequest): Promise<HttpResponse> {
    const resp = await nodefetch(request.url, {
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
