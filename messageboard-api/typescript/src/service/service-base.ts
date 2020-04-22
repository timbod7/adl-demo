import { HttpFetch, HttpRequest } from "./http";
import * as ADL from "../adl/runtime/adl";
import { HttpPost } from "../adl/types";
import { createJsonBinding, JsonBinding } from "../adl/runtime/json";

export class ServiceBase {

  constructor(
    private readonly http: HttpFetch,
    private readonly baseUrl: string,
    private readonly resolver: ADL.DeclResolver,
  ) {
  }

  mkPostFn<I, O>(rtype: HttpPost<I, O>): ReqFn<I, O> {
    const bb = createBiBinding<I, O>(this.resolver, rtype);
    return (req: I) => {
      const jsonArgs = bb.reqJB.toJson(req);
      return this.requestAdl("post", rtype.path, jsonArgs, bb.respJB, undefined);
    };
  }

  mkAuthPostFn<I, O>(rtype: HttpPost<I, O>): AuthReqFn<I, O> {
    const bb = createBiBinding<I, O>(this.resolver, rtype);
    return (authToken:string, req: I) => {
      const jsonArgs = bb.reqJB.toJson(req);
      return this.requestAdl("post", rtype.path, jsonArgs, bb.respJB, authToken);
    };
  }

  private async requestAdl<O>(
    method: "get" | "post",
    path: string,
    jsonArgs: {} | null,
    respJB: JsonBinding<O>,
    authToken: string | undefined,
  ): Promise<O> {
    // Construct request
    const headers: { [key: string]: string } = {};
    if (authToken) {
      headers["Authorization"] = "Bearer " + authToken;
    }
    const httpReq: HttpRequest = {
      url: this.baseUrl + path,
      headers,
      method,
      body: jsonArgs ? JSON.stringify(jsonArgs) : undefined
    };

    // Make request
    const resp = await this.http.fetch(httpReq);

    // Check for errors
    if (!resp.ok) {
      throw new Error(
        `Encountered server error attempting ${httpReq.method} request to ${httpReq.url} failed: ${resp.status} ${resp.statusText}`
      );
    }

    // Parse and response
    const respJson = await resp.json();
    return respJB.fromJsonE(respJson);
  }
}

export type ReqFn<I, O> = (req: I) => Promise<O>;

export type AuthReqFn<I, O> = (authToken: string, req: I) => Promise<O>;

interface BiTypeExpr<I, O> {
  reqType: ADL.ATypeExpr<I>;
  respType: ADL.ATypeExpr<O>;
}

interface BiBinding<I, O> {
  reqJB: JsonBinding<I>;
  respJB: JsonBinding<O>;
}

function createBiBinding<I, O>(resolver: ADL.DeclResolver, rtype: BiTypeExpr<I, O>): BiBinding<I, O> {
  return {
    reqJB: createJsonBinding(resolver, rtype.reqType),
    respJB: createJsonBinding(resolver, rtype.respType)
  };
}

