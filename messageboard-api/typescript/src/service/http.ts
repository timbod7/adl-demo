/** A minimal http abstraction */

export interface HttpFetch {
  fetch(request: HttpRequest): Promise<HttpResponse>;
}

export interface HttpHeaders {
  [index: string]: string;
}

export interface HttpRequest {
  url: string;
  headers: HttpHeaders;
  method: "get" | "put" | "post";
  body?: string;
}

export interface HttpResponse {
  status: number;
  statusText: string;
  ok: boolean;
  text(): Promise<string>;
  json(): Promise<{} | null>;
}
