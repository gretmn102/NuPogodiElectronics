(function(){const t=document.createElement("link").relList;if(t&&t.supports&&t.supports("modulepreload"))return;for(const o of document.querySelectorAll('link[rel="modulepreload"]'))r(o);new MutationObserver(o=>{for(const s of o)if(s.type==="childList")for(const u of s.addedNodes)u.tagName==="LINK"&&u.rel==="modulepreload"&&r(u)}).observe(document,{childList:!0,subtree:!0});function n(o){const s={};return o.integrity&&(s.integrity=o.integrity),o.referrerPolicy&&(s.referrerPolicy=o.referrerPolicy),o.crossOrigin==="use-credentials"?s.credentials="include":o.crossOrigin==="anonymous"?s.credentials="omit":s.credentials="same-origin",s}function r(o){if(o.ep)return;o.ep=!0;const s=n(o);fetch(o.href,s)}})();const dn=""+new URL("assets-eebd4b35.svg",import.meta.url).href;function $(e){return Array.isArray(e)||ArrayBuffer.isView(e)}function pn(e){return e!=null&&typeof e.GetEnumerator=="function"}function yn(e){return e!=null&&typeof e.CompareTo=="function"}function _n(e){return e!=null&&typeof e.Equals=="function"}function bn(e){return e!=null&&typeof e.GetHashCode=="function"}function wn(e){return e!=null&&typeof e.Dispose=="function"}function O(e){wn(e)&&e.Dispose()}function Z(){return null}function pe(e,t){var n,r;return((n=Object.getPrototypeOf(e))==null?void 0:n.constructor)===((r=Object.getPrototypeOf(t))==null?void 0:r.constructor)}class En{constructor(t){this.iter=t,this.current=Z()}"System.Collections.Generic.IEnumerator`1.get_Current"(){return this.current}"System.Collections.IEnumerator.get_Current"(){return this.current}"System.Collections.IEnumerator.MoveNext"(){const t=this.iter.next();return this.current=t.value,!t.done}"System.Collections.IEnumerator.Reset"(){throw new Error("JS iterators cannot be reset")}Dispose(){}}function k(e){return pn(e)?e.GetEnumerator():new En(e[Symbol.iterator]())}function Ve(e){return{next(){const t=e["System.Collections.IEnumerator.MoveNext"](),n=t?e["System.Collections.Generic.IEnumerator`1.get_Current"]():void 0;return{done:!t,value:n}}}}function v(e,t){let n=e.toString(10);for(;n.length<t;)n="0"+n;return n}function Je(e){const t=e;return typeof t.offset=="number"?t.offset:e.kind===1?0:e.getTimezoneOffset()*-6e4}function De(e,t){return e=e<0&&t!=null&&t!==10?4294967295+e+1:e,e.toString(t)}class W{static id(t){return W.idMap.has(t)||W.idMap.set(t,++W.count),W.idMap.get(t)}}W.idMap=new WeakMap;W.count=0;function Re(e){let t=0,n=5381;const r=e.length;for(;t<r;)n=n*33^e.charCodeAt(t++);return n}function Le(e){return e*2654435761|0}function Cn(e){return Re(e.toString(32))}function ae(e){let t=0;const n=e.length;for(let r=0;r<n;r++){const o=e[r];t=(t<<5)+t^o}return t}function Sn(e){return e.getTime()}function Mn(e){const t=e.length,n=new Array(t);for(let r=0;r<t;r++)n[r]=V(e[r]);return ae(n)}function V(e){var t;if(e==null)return 0;switch(typeof e){case"boolean":return e?1:0;case"number":return Le(e);case"bigint":return Cn(e);case"string":return Re(e);default:{if(bn(e))return e.GetHashCode();if($(e))return Mn(e);if(e instanceof Date)return Sn(e);if(((t=Object.getPrototypeOf(e))==null?void 0:t.constructor)===Object){const n=Object.values(e).map(r=>V(r));return ae(n)}else return Le(W.id(e))}}}function Fe(e,t,n){if(e==null)return t==null;if(t==null||e.length!==t.length)return!1;for(let r=0;r<e.length;r++)if(!n(e[r],t[r]))return!1;return!0}function it(e,t){return Fe(e,t,F)}function Tn(e,t){const n=Object.keys(e),r=Object.keys(t);if(n.length!==r.length)return!1;n.sort(),r.sort();for(let o=0;o<n.length;o++)if(n[o]!==r[o]||!F(e[n[o]],t[r[o]]))return!1;return!0}function F(e,t){var n;return e===t?!0:e==null?t==null:t==null?!1:_n(e)?e.Equals(t):$(e)?$(t)&&it(e,t):typeof e!="object"?!1:e instanceof Date?t instanceof Date&&lt(e,t)===0:((n=Object.getPrototypeOf(e))==null?void 0:n.constructor)===Object&&Tn(e,t)}function lt(e,t){let n,r;return"offset"in e&&"offset"in t?(n=e.getTime(),r=t.getTime()):(n=e.getTime()+Je(e),r=t.getTime()+Je(t)),n===r?0:n<r?-1:1}function te(e,t){return e===t?0:e<t?-1:1}function An(e,t,n){if(e==null)return t==null?0:1;if(t==null)return-1;if(e.length!==t.length)return e.length<t.length?-1:1;for(let r=0,o=0;r<e.length;r++)if(o=n(e[r],t[r]),o!==0)return o;return 0}function ct(e,t){return An(e,t,D)}function In(e,t){const n=Object.keys(e),r=Object.keys(t);if(n.length!==r.length)return n.length<r.length?-1:1;n.sort(),r.sort();for(let o=0,s=0;o<n.length;o++){const u=n[o];if(u!==r[o])return u<r[o]?-1:1;if(s=D(e[u],t[u]),s!==0)return s}return 0}function D(e,t){var n;return e===t?0:e==null?t==null?0:-1:t==null?1:yn(e)?e.CompareTo(t):$(e)?$(t)?ct(e,t):-1:typeof e!="object"?e<t?-1:1:e instanceof Date?t instanceof Date?lt(e,t):-1:((n=Object.getPrototypeOf(e))==null?void 0:n.constructor)===Object?In(e,t):-1}function Me(e){let t=e;return(...n)=>{if(n.length===0)return t;t=n[0]}}function kn(e){let t=0,n="[";for(const r of e){if(t===0)n+=B(r);else if(t===100){n+="; ...";break}else n+="; "+B(r);t++}return n+"]"}function B(e,t=0){var n;if(e!=null&&typeof e=="object"){if(typeof e.toString=="function")return e.toString();if(Symbol.iterator in e)return kn(e);{const r=(n=Object.getPrototypeOf(e))==null?void 0:n.constructor;return r===Object&&t<10?"{ "+Object.entries(e).map(([o,s])=>o+" = "+B(s,t+1)).join(`
  `)+" }":(r==null?void 0:r.name)??""}}return String(e)}function Nn(e,t){if(t.length===0)return e;{let n,r=!0;return t.length===1?(n=B(t[0]),r=n.indexOf(" ")>=0):n=t.map(o=>B(o)).join(", "),e+(r?" (":" ")+n+(r?")":"")}}class je{get name(){return this.cases()[this.tag]}toJSON(){return this.fields.length===0?this.name:[this.name].concat(this.fields)}toString(){return Nn(this.name,this.fields)}GetHashCode(){const t=this.fields.map(n=>V(n));return t.splice(0,0,Le(this.tag)),ae(t)}Equals(t){return this===t?!0:pe(this,t)&&this.tag===t.tag?it(this.fields,t.fields):!1}CompareTo(t){return this===t?0:pe(this,t)?this.tag===t.tag?ct(this.fields,t.fields):this.tag<t.tag?-1:1:-1}}function vn(e){const t={},n=Object.keys(e);for(let r=0;r<n.length;r++)t[n[r]]=e[n[r]];return t}function On(e){return"{ "+Object.entries(e).map(([t,n])=>t+" = "+B(n)).join(`
  `)+" }"}function Ln(e){const t=Object.values(e).map(n=>V(n));return ae(t)}function Fn(e,t){if(e===t)return!0;if(pe(e,t)){const n=Object.keys(e);for(let r=0;r<n.length;r++)if(!F(e[n[r]],t[n[r]]))return!1;return!0}else return!1}function Bn(e,t){if(e===t)return 0;if(pe(e,t)){const n=Object.keys(e);for(let r=0;r<n.length;r++){const o=D(e[n[r]],t[n[r]]);if(o!==0)return o}return 0}else return-1}class K{toJSON(){return vn(this)}toString(){return On(this)}GetHashCode(){return Ln(this)}Equals(t){return Fn(this,t)}CompareTo(t){return Bn(this,t)}}const Y=Symbol("numeric");function at(e){return typeof e=="number"||typeof e=="bigint"||(e==null?void 0:e[Y])}function Pn(e,t){return typeof e=="number"||typeof e=="bigint"?e<t?-1:e>t?1:0:e.CompareTo(t)}function Be(e,t){return typeof e=="number"?e*t:typeof e=="bigint"?e*BigInt(t):e[Y]().multiply(t)}function ge(e,t){return typeof e=="number"?e.toFixed(t):typeof e=="bigint"?e:e[Y]().toFixed(t)}function ye(e,t){return typeof e=="number"?e.toPrecision(t):typeof e=="bigint"?e:e[Y]().toPrecision(t)}function _e(e,t){return typeof e=="number"?e.toExponential(t):typeof e=="bigint"?e:e[Y]().toExponential(t)}function be(e){return typeof e=="number"?(Number(e)>>>0).toString(16):typeof e=="bigint"?BigInt.asUintN(64,e).toString(16):e[Y]().toHex()}class ft{constructor(t,n,r,o,s,u,i){this.fullname=t,this.generics=n,this.construct=r,this.parent=o,this.fields=s,this.cases=u,this.enumCases=i}toString(){return We(this)}GetHashCode(){return ht(this)}Equals(t){return Ge(this,t)}}function Pe(e){return e.generics!=null?e.generics:[]}function ht(e){const t=Re(e.fullname),n=Pe(e).map(ht);return ae([t,...n])}function Ge(e,t){return e.fullname===""?t.fullname===""&&Fe(Xe(e),Xe(t),([n,r],[o,s])=>n===o&&Ge(r,s)):e.fullname===t.fullname&&Fe(Pe(e),Pe(t),Ge)}function mt(e,t,n){return new ft(e,[t],void 0,void 0,void 0,void 0,n)}const gt=new ft("System.Int32");function We(e){const t=Gn(e);return t!=null?We(t)+"[]":e.generics==null||e.generics.length===0?e.fullname:e.fullname+"["+e.generics.map(n=>We(n)).join(",")+"]"}function Gn(e){var t;return e.fullname==="[]"&&((t=e.generics)==null?void 0:t.length)===1?e.generics[0]:void 0}function Wn(e){return e.enumCases!=null&&e.enumCases.length>0}function dt(e){if(Wn(e)&&e.enumCases!=null)return e.enumCases.map(t=>t[1]);throw new Error(`${e.fullname} is not an enum type`)}function Xe(e){if(e.fields!=null)return e.fields();throw new Error(`${e.fullname} is not an F# record type`)}class x{constructor(t){this.value=t}toJSON(){return this.value}toString(){return String(this.value)}GetHashCode(){return V(this.value)}Equals(t){return t==null?!1:F(this.value,t instanceof x?t.value:t)}CompareTo(t){return t==null?1:D(this.value,t instanceof x?t.value:t)}}function a(e){if(e==null)throw new Error("Option has no value");return e instanceof x?e.value:e}function qe(e){return e==null||e instanceof x?new x(e):e}function Hn(e){return e??void 0}function $n(e,t){return e!=null?a(e):t()}function Ue(e){const t=e<0;e=Math.abs(e);const n=~~(e/36e5),r=e%36e5/6e4;return(t?"-":"+")+v(n,2)+":"+v(r,2)}function we(e,t){const n=e.toISOString();return t==="first"?n.substring(0,n.indexOf("T")):n.substring(n.indexOf("T")+1,n.length-1)}function Vn(e,t){if(t)return e.toISOString();{const n=e.kind==null?!0:e.kind===2;return v(e.getFullYear(),4)+"-"+v(e.getMonth()+1,2)+"-"+v(e.getDate(),2)+"T"+v(e.getHours(),2)+":"+v(e.getMinutes(),2)+":"+v(e.getSeconds(),2)+"."+v(e.getMilliseconds(),3)+(n?Ue(e.getTimezoneOffset()*-6e4):"")}}function Dn(e,t){const n=e.toISOString();return n.substring(0,n.length-1)+Ue(t)}function pt(e,t,n){return t.replace(/(\w)\1*/g,r=>{let o=Number.NaN;switch(r.substring(0,1)){case"y":const s=n?e.getUTCFullYear():e.getFullYear();o=r.length<4?s%100:s;break;case"M":o=(n?e.getUTCMonth():e.getMonth())+1;break;case"d":o=n?e.getUTCDate():e.getDate();break;case"H":o=n?e.getUTCHours():e.getHours();break;case"h":const u=n?e.getUTCHours():e.getHours();o=u>12?u%12:u;break;case"m":o=n?e.getUTCMinutes():e.getMinutes();break;case"s":o=n?e.getUTCSeconds():e.getSeconds();break;case"f":o=n?e.getUTCMilliseconds():e.getMilliseconds();break}return Number.isNaN(o)?r:v(o,r.length)})}function Rn(e,t){const n=new Date(e.getTime()+(e.offset??0));if(typeof t!="string")return n.toISOString().replace(/\.\d+/,"").replace(/[A-Z]|\.\d+/g," ")+Ue(e.offset??0);if(t.length===1)switch(t){case"D":case"d":return we(n,"first");case"T":case"t":return we(n,"second");case"O":case"o":return Dn(n,e.offset??0);default:throw new Error("Unrecognized Date print format")}else return pt(n,t,!0)}function jn(e,t){const n=e.kind===1;if(typeof t!="string")return n?e.toUTCString():e.toLocaleString();if(t.length===1)switch(t){case"D":case"d":return n?we(e,"first"):e.toLocaleDateString();case"T":case"t":return n?we(e,"second"):e.toLocaleTimeString();case"O":case"o":return Vn(e,n);default:throw new Error("Unrecognized Date print format")}else return pt(e,t,n)}function yt(e,t,n){return e.offset!=null?Rn(e,t):jn(e,t)}const Q=/(^|[^%])%([0+\- ]*)(\*|\d+)?(?:\.(\d+))?(\w)/g,qn=/\{(\d+)(,-?\d+)?(?:\:([a-zA-Z])(\d{0,2})|\:(.+?))?\}/g;function _t(e,t){return Pn(e,t)<0}function A(e){return{input:e,cont:zn(e)}}function bt(e,t){return typeof t=="string"?e(t):t.cont(e)}function Un(e){return bt(t=>t,e)}function I(e){return bt(t=>{throw new Error(t)},e)}function Kn(e,t,n,r,o){let s="";if(t=t||"",o=o||"",at(e))switch(o.toLowerCase()!=="x"&&(_t(e,0)?(e=Be(e,-1),s="-"):t.indexOf(" ")>=0?s=" ":t.indexOf("+")>=0&&(s="+")),r=r==null?null:parseInt(r,10),o){case"f":case"F":r=r??6,e=ge(e,r);break;case"g":case"G":e=r!=null?ye(e,r):ye(e);break;case"e":case"E":e=r!=null?_e(e,r):_e(e);break;case"x":e=be(e);break;case"X":e=be(e).toUpperCase();break;default:e=String(e);break}else e instanceof Date?e=yt(e):e=B(e);if(n=typeof n=="number"?n:parseInt(n,10),isNaN(n))e=s+e;else{const u=t.indexOf("0")>=0,i=t.indexOf("-")>=0,c=i||!u?" ":"0";c==="0"?(e=oe(e,n-s.length,c,i),e=s+e):e=oe(s+e,n,c,i)}return e}function wt(e,t,n,r="",o=-1){return(...s)=>{let u=r;const i=t.slice(),c=n.slice();for(const l of s){const[,,m,g,y,h]=c[0];let d=g;if(o>=0)d=o,o=-1;else if(d==="*"){if(l<0)throw new Error("Non-negative number required");o=l;continue}u+=i[0],u+=Kn(l,m,d,y,h),i.splice(0,1),c.splice(0,1)}return c.length===0?(u+=i[0],e(u)):wt(e,i,c,u,o)}}function zn(e){return t=>{Q.lastIndex=0;const n=[],r=[];let o=0,s=Q.exec(e);for(;s;){const u=s.index+(s[1]||"").length;n.push(e.substring(o,u).replace(/%%/g,"%")),r.push(s),o=Q.lastIndex,Q.lastIndex-=1,s=Q.exec(e)}return n.length===0?t(e.replace(/%%/g,"%")):(n.push(e.substring(o).replace(/%%/g,"%")),wt(t,n,r))}}function Zn(e,...t){let n;return typeof e=="object"?(n=String(t[0]),t.shift()):n=e,n.replace(qn,(r,o,s,u,i,c)=>{if(o<0||o>=t.length)throw new Error("Index must be greater or equal to zero and less than the arguments' length.");let l=t[o];if(at(l))switch(i=i==null?null:parseInt(i,10),u){case"f":case"F":i=i??2,l=ge(l,i);break;case"g":case"G":l=i!=null?ye(l,i):ye(l);break;case"e":case"E":l=i!=null?_e(l,i):_e(l);break;case"p":case"P":i=i??2,l=ge(Be(l,100),i)+" %";break;case"d":case"D":l=i!=null?Ae(String(l),i,"0"):String(l);break;case"x":case"X":l=i!=null?Ae(be(l),i,"0"):be(l),u==="X"&&(l=l.toUpperCase());break;default:if(c){let m="";l=c.replace(/([0#,]+)(\.[0#]+)?/,(g,y,h)=>{_t(l,0)&&(l=Be(l,-1),m="-"),h=h==null?"":h.substring(1),l=ge(l,Math.max(h.length,0));let[d,p]=l.split(".");p||(p="");const w=y.replace(/,/g,"").replace(/^#+/,"").length;d=Ae(d,w,"0");const f=h.replace(/#+$/,"").length;if(f>p.length?p=xn(p,f,"0"):f<p.length&&(p=p.substring(0,f)+p.substring(f).replace(/0+$/,"")),y.indexOf(",")>0){const E=d.length%3,C=Math.floor(d.length/3);let z=E>0?d.substr(0,E)+(C>0?",":""):"";for(let P=0;P<C;P++)z+=d.substr(E+P*3,3)+(P<C-1?",":"");d=z}return p.length>0?d+"."+p:d}),l=m+l}}else l instanceof Date?l=yt(l,c||u):l=B(l);return s=parseInt((s||" ").substring(1),10),isNaN(s)||(l=oe(String(l),Math.abs(s)," ",s<0)),l})}function Et(e,t){return Array.isArray(t)?t.join(e):Array.from(t).join(e)}function oe(e,t,n,r){n=n||" ",t=t-e.length;for(let o=0;o<t;o++)e=r?e+n:n+e;return e}function Ae(e,t,n){return oe(e,t,n)}function xn(e,t,n){return oe(e,t,n,!0)}function Jn(e){throw new Error(e)}function Xn(e,t){return typeof e=="function"?new e(t):new Array(t)}const Ct="Collection was empty.";function St(e,t,n){const r=t.length|0,o=Xn(n,r);for(let s=0;s<=r-1;s++)o[s]=e(t[s]);return o}function Yn(e,t,n){const r=[];for(let o=0;o<=t.length-1;o++){const s=e(t[o]);if(s!=null){const u=a(s);r.push(u)}}return F(n,Z())?r:St(o=>o,r,n)}function Qn(e,t,n){return n.reduce(e,t)}class q extends K{constructor(t,n){super(),this.head=t,this.tail=n}toString(){return"["+Et("; ",this)+"]"}Equals(t){const n=this;return n===t?!0:((o,s)=>{e:for(;;){const u=o,i=s,c=u.tail,l=i.tail;if(c!=null)if(l!=null){const m=a(c),g=a(l);if(F(u.head,i.head)){o=m,s=g;continue e}else return!1}else return!1;else return l==null}})(n,t)}GetHashCode(){return((r,o,s)=>{e:for(;;){const u=r,i=o,c=s,l=c.tail;if(l!=null){const m=a(l);if(u>18)return i|0;r=u+1,o=(i<<1)+V(c.head)+631*u,s=m;continue e}else return i|0}})(0,0,this)|0}toJSON(){const t=this;return Array.from(t)}CompareTo(t){return((o,s)=>{e:for(;;){const u=o,i=s,c=u.tail,l=i.tail;if(c!=null)if(l!=null){const m=a(c),g=a(l),y=D(u.head,i.head)|0;if(y===0){o=m,s=g;continue e}else return y|0}else return 1;else return l!=null?-1:0}})(this,t)|0}GetEnumerator(){return tr(this)}[Symbol.iterator](){return Ve(k(this))}"System.Collections.IEnumerable.GetEnumerator"(){return k(this)}}class er{constructor(t){this.xs=t,this.it=this.xs,this.current=Z()}"System.Collections.Generic.IEnumerator`1.get_Current"(){return this.current}"System.Collections.IEnumerator.get_Current"(){return this.current}"System.Collections.IEnumerator.MoveNext"(){const t=this,n=t.it.tail;if(n!=null){const r=a(n);return t.current=t.it.head,t.it=r,!0}else return!1}"System.Collections.IEnumerator.Reset"(){const t=this;t.it=t.xs,t.current=Z()}Dispose(){}}function tr(e){return new er(e)}function U(){return new q(Z(),void 0)}function Mt(e,t){return new q(e,t)}function Tt(e){return e.tail==null}function nr(e){if(e.tail!=null)return e.head;throw new Error(Ct+"\\nParameter name: list")}function Te(e){const t=e.tail;if(t!=null)return a(t);throw new Error(Ct+"\\nParameter name: list")}function At(){return U()}function rr(e){return Mt(e,U())}function ne(e){return Tt(e)}function J(e){return nr(e)}function It(e){return Te(e)}function kt(e,t,n){let r=t,o=n;for(;!Tt(o);)r=e(r,J(o)),o=Te(o);return r}function Nt(e,t){let n=t;for(let r=e.length-1;r>=0;r--)n=Mt(e[r],n);return n}function Ke(e){return Nt(e,U())}function or(e){let t,n;if($(e))return Ke(e);if(e instanceof q)return e;{const r=U();let o=r;const s=k(e);try{for(;s["System.Collections.IEnumerator.MoveNext"]();){const c=s["System.Collections.Generic.IEnumerator`1.get_Current"]();o=(t=o,n=new q(c,void 0),t.tail=n,n)}}finally{O(s)}const u=o,i=U();return u.tail=i,Te(r)}}function sr(e,t){const n=U(),r=kt((s,u)=>{const i=new q(e(u),void 0);return s.tail=i,i},n,t),o=U();return r.tail=o,Te(n)}const ur="Enumeration already finished.",ir="Enumeration has not started. Call MoveNext.",lr="Reset is not supported on this enumerator.";function cr(){throw new Error(lr)}function vt(){throw new Error(ir)}function ar(){throw new Error(ur)}class fr{constructor(t){this.f=t}toString(){const t=this;let n=0,r="seq [";const o=k(t);try{for(;n<4&&o["System.Collections.IEnumerator.MoveNext"]();)n>0&&(r=r+"; "),r=r+B(o["System.Collections.Generic.IEnumerator`1.get_Current"]()),n=n+1|0;return n===4&&(r=r+"; ..."),r+"]"}finally{O(o)}}GetEnumerator(){return this.f()}[Symbol.iterator](){return Ve(k(this))}"System.Collections.IEnumerable.GetEnumerator"(){return this.f()}}function hr(e){return new fr(e)}class mr{constructor(t,n,r){this.current=t,this.next=n,this.dispose=r}"System.Collections.Generic.IEnumerator`1.get_Current"(){return this.current()}"System.Collections.IEnumerator.get_Current"(){return this.current()}"System.Collections.IEnumerator.MoveNext"(){return this.next()}"System.Collections.IEnumerator.Reset"(){cr()}Dispose(){this.dispose()}}function Ot(e,t,n){return new mr(e,t,n)}function gr(e,t,n){let r=!1,o,s=qe(e());const u=()=>{if(s!=null){const c=a(s);try{n(c)}finally{s=void 0}}},i=()=>{try{u()}finally{o=void 0}};return Ot(()=>(r||vt(),o!=null?a(o):ar()),()=>{if(r||(r=!0),s!=null){const c=a(s);let l;try{l=t(c)}catch(m){throw i(),m}return l!=null?(o=l,!0):(i(),!1)}else return!1},u)}function dr(e,t){let n,r=t;return Ot(()=>{if(n!=null){const o=a(n)[0];return a(n)[1],o}else return vt()},()=>(n=e(r),n!=null?(a(n)[0],r=a(n)[1],!0):!1),()=>{})}function pr(e,t){t==null&&Jn(e)}function ze(e){return hr(e)}function Ee(e){return pr("source",e),k(e)}function yr(e){return ze(()=>k(e()))}function _r(e,t){return ze(()=>dr(e,t))}function br(e){return $(e)?Ke(e):e instanceof q?e:or(e)}function wr(e,t,n){return ze(()=>gr(e,t,n))}function Er(e,t,n){const r=Ee(t);try{const o=Ee(n);try{let s=0,u=r["System.Collections.IEnumerator.MoveNext"](),i=o["System.Collections.IEnumerator.MoveNext"]();for(;s===0&&u&&i;)s=e(r["System.Collections.Generic.IEnumerator`1.get_Current"](),o["System.Collections.Generic.IEnumerator`1.get_Current"]())|0,s===0&&(u=r["System.Collections.IEnumerator.MoveNext"](),i=o["System.Collections.IEnumerator.MoveNext"]());return(s!==0?s:u?1:i?-1:0)|0}finally{O(o)}}finally{O(r)}}function Cr(e,t,n){const r=Ee(n);try{let o=t;for(;r["System.Collections.IEnumerator.MoveNext"]();)o=e(o,r["System.Collections.Generic.IEnumerator`1.get_Current"]());return o}finally{O(r)}}function Sr(e,t){Cr((n,r)=>{e(r)},void 0,t)}function he(e,t){return wr(()=>Ee(t),n=>n["System.Collections.IEnumerator.MoveNext"]()?qe(e(n["System.Collections.Generic.IEnumerator`1.get_Current"]())):void 0,n=>{O(n)})}const R={None:0,LowerFirst:1,SnakeCase:2,SnakeCaseAllCaps:3,KebabCase:4};function Ie(e,t){return e.replace(/[a-z]?[A-Z]/g,n=>n.length===1?n.toLowerCase():n.charAt(0)+t+n.charAt(1).toLowerCase())}function Mr(e,t){switch(t){case R.LowerFirst:return e.charAt(0).toLowerCase()+e.slice(1);case R.SnakeCase:return Ie(e,"_");case R.SnakeCaseAllCaps:return Ie(e,"_").toUpperCase();case R.KebabCase:return Ie(e,"-");case R.None:default:return e}}function Tr(e,t=R.None){const n={},r=t;function o(u){throw new Error("Cannot infer key and value of "+String(u))}function s(u,i,c){u=Mr(u,i),n[u]=c}for(let u of e){let i=R.None;if(u==null&&o(u),u instanceof je){const c=u.cases()[u.tag];u=u.fields.length===0?c:[c].concat(u.fields),i=r}if(Array.isArray(u))switch(u.length){case 0:o(u);break;case 1:s(u[0],i,!0);break;case 2:const c=u[1];s(u[0],i,c);break;default:s(u[0],i,u.slice(1))}else typeof u=="string"?s(u,i,!0):o(u)}return n}class Ye extends je{constructor(t,n){super(),this.tag=t,this.fields=n}cases(){return["Ok","Error"]}}function Ar(e){return De(e.status)+" "+e.statusText+" for URL "+e.url}function Ir(e,t){return fetch(e,Tr(t,1)).then(r=>{if(r.ok)return r;throw new Error(Ar(r))})}class Lt{constructor(t,n){this.k=t,this.v=n}}function se(e,t){return new Lt(e,t)}function b(e){return e.k}function T(e){return e.v}class _ extends Lt{constructor(t,n,r,o,s){super(t,n),this.left=r,this.right=o,this.h=s|0}}function re(e,t,n,r,o){return new _(e,t,n,r,o)}function S(e){return e.left}function M(e){return e.right}function H(e){return e.h}function Ft(e,t){e:for(;;){const n=e,r=t;if(r!=null){const o=a(r);if(o instanceof _){const s=o;e=Ft(n+1,S(s)),t=M(s);continue e}else return n+1|0}else return n|0}}function kr(e){return Ft(0,e)}function N(e,t,n,r){let o,s,u;const i=e;if(i!=null){const g=a(i);u=g instanceof _?(o=g,H(o)):1}else u=0;let c;const l=r;if(l!=null){const g=a(l);c=g instanceof _?(s=g,H(s)):1}else c=0;const m=(u<c?c:u)|0;return m===0?se(t,n):re(t,n,e,r,m+1)}function Qe(e,t,n,r){let o,s,u,i,c,l,m,g,y;const h=e;if(h!=null){const w=a(h);y=w instanceof _?(o=w,H(o)):1}else y=0;let d;const p=r;if(p!=null){const w=a(p);d=w instanceof _?(s=w,H(s)):1}else d=0;if(d>y+2){const w=a(r);if(w instanceof _){const f=w;if(u=S(f),(u!=null?(i=a(u),i instanceof _?(c=i,H(c)):1):0)>y+1){const E=a(S(f));if(E instanceof _){const C=E;return N(N(e,t,n,S(C)),b(C),T(C),N(M(C),b(f),T(f),M(f)))}else throw new Error("internal error: Map.rebalance")}else return N(N(e,t,n,S(f)),b(f),T(f),M(f))}else throw new Error("internal error: Map.rebalance")}else if(y>d+2){const w=a(e);if(w instanceof _){const f=w;if(l=M(f),(l!=null?(m=a(l),m instanceof _?(g=m,H(g)):1):0)>d+1){const E=a(M(f));if(E instanceof _){const C=E;return N(N(S(f),b(f),T(f),S(C)),b(C),T(C),N(M(C),t,n,r))}else throw new Error("internal error: Map.rebalance")}else return N(S(f),b(f),T(f),N(M(f),t,n,r))}else throw new Error("internal error: Map.rebalance")}else return N(e,t,n,r)}function X(e,t,n,r){if(r!=null){const o=a(r),s=e.Compare(t,b(o))|0;if(o instanceof _){const u=o;return s<0?Qe(X(e,t,n,S(u)),b(u),T(u),M(u)):s===0?re(t,n,S(u),M(u),H(u)):Qe(S(u),b(u),T(u),X(e,t,n,M(u)))}else return s<0?re(t,n,void 0,r,2):s===0?se(t,n):re(t,n,r,void 0,2)}else return se(t,n)}function Bt(e,t,n){e:for(;;){const r=e,o=t,s=n;if(s!=null){const u=a(s),i=r.Compare(o,b(u))|0;if(i===0)return qe(T(u));if(u instanceof _){const c=u;e=r,t=o,n=i<0?S(c):M(c);continue e}else return}else return}}function Nr(e,t,n){const r=Bt(e,t,n);if(r==null)throw new Error;return a(r)}function vr(e,t,n){e:for(;;){const r=e,o=t,s=n;if(s!=null){const u=a(s),i=r.Compare(o,b(u))|0;if(u instanceof _){const c=u;if(i<0){e=r,t=o,n=S(c);continue e}else{if(i===0)return!0;e=r,t=o,n=M(c);continue e}}else return i===0}else return!1}}function Pt(e,t){e:for(;;){const n=e,r=t;if(r!=null){const o=a(r);if(o instanceof _){const s=o;Pt(n,S(s)),n(b(s),T(s)),e=n,t=M(s);continue e}else n(b(o),T(o))}break}}function Gt(e,t){Pt(e,t)}function He(e,t){if(t!=null){const n=a(t);if(n instanceof _){const r=n,o=He(e,S(r)),s=e(b(r),T(r)),u=He(e,M(r));return re(b(r),s,o,u,H(r))}else return se(b(n),e(b(n),T(n)))}else return void 0}function Or(e,t){return He(e,t)}function Wt(e,t,n){e:for(;;){const r=e,o=t,s=n;if(s!=null){const u=a(s);if(u instanceof _){const i=u;e=r,t=r(Wt(r,o,S(i)),b(i),T(i)),n=M(i);continue e}else return r(o,b(u),T(u))}else return o}}function Lr(e,t,n){return Wt(e,t,n)}function Fr(e,t,n){let r=n;Gt((o,s)=>{t[r]=[o,s],r=r+1|0},e)}function Br(e,t){return kt((n,r)=>X(e,r[0],r[1],n),void 0,t)}function Pr(e,t,n){e:for(;;){const r=e,o=t,s=n;if(s["System.Collections.IEnumerator.MoveNext"]()){const u=s["System.Collections.Generic.IEnumerator`1.get_Current"]();e=r,t=X(r,u[0],u[1],o),n=s;continue e}else return o}}function Gr(e,t){let n=void 0;for(let r=0;r<=t.length-1;r++){const o=t[r];n=X(e,o[0],o[1],n)}return n}function Ht(e,t){if($(t))return Gr(e,t);if(t instanceof q)return Br(e,t);{const n=k(t);try{return Pr(e,void 0,n)}finally{O(n)}}}class Wr extends K{constructor(t,n){super(),this.stack=t,this.started=n}}function $t(e){e:for(;;){const t=e;if(ne(t))return At();{const n=It(t),r=J(t);if(r!=null){const o=a(r);if(o instanceof _){const s=o;e=Nt([S(s),se(b(s),T(s)),M(s)],n);continue e}else return t}else{e=n;continue e}}}}function et(e){return new Wr($t(rr(e)),!1)}function Hr(){throw new Error("enumeration not started")}function $r(){throw new Error("enumeration already finished")}function tt(e){if(e.started){const t=e.stack;if(ne(t))return $r();if(J(t)!=null){const n=a(J(t));if(n instanceof _)throw new Error("Please report error: Map iterator, unexpected stack for current");return[b(n),T(n)]}else throw new Error("Please report error: Map iterator, unexpected stack for current")}else return Hr()}function Vr(e){if(e.started){const t=e.stack;if(ne(t))return!1;if(J(t)!=null){if(a(J(t))instanceof _)throw new Error("Please report error: Map iterator, unexpected stack for moveNext");return e.stack=$t(It(t)),!ne(e.stack)}else throw new Error("Please report error: Map iterator, unexpected stack for moveNext")}else return e.started=!0,!ne(e.stack)}function nt(e){let t=et(e);return{"System.Collections.Generic.IEnumerator`1.get_Current"(){return tt(t)},"System.Collections.IEnumerator.get_Current"(){return tt(t)},"System.Collections.IEnumerator.MoveNext"(){return Vr(t)},"System.Collections.IEnumerator.Reset"(){t=et(e)},Dispose(){}}}class Ce{constructor(t,n){this.comparer=t,this.tree=n}GetHashCode(){return zr(this)|0}Equals(t){const n=this;if(t instanceof Ce){const r=t,o=k(n);try{const s=k(r);try{const u=()=>{const i=o["System.Collections.IEnumerator.MoveNext"]();if(i===s["System.Collections.IEnumerator.MoveNext"]())if(i){const c=o["System.Collections.Generic.IEnumerator`1.get_Current"](),l=s["System.Collections.Generic.IEnumerator`1.get_Current"]();return F(c[0],l[0])&&F(c[1],l[1])?u():!1}else return!0;else return!1};return u()}finally{O(s)}}finally{O(o)}}else return!1}toString(){return"map ["+Et("; ",he(n=>Zn("({0}, {1})",n[0],n[1]),this))+"]"}get[Symbol.toStringTag](){return"FSharpMap"}toJSON(){const t=this;return Array.from(t)}GetEnumerator(){return nt(this.tree)}[Symbol.iterator](){return Ve(k(this))}"System.Collections.IEnumerable.GetEnumerator"(){return nt(this.tree)}CompareTo(t){const n=this;if(t instanceof Ce)return Er((o,s)=>{const u=n.comparer.Compare(o[0],s[0])|0;return(u!==0?u:D(o[1],s[1]))|0},n,t)|0;throw new Error("not comparable\\nParameter name: obj")}"System.Collections.Generic.ICollection`1.Add2B595"(t){throw new Error("Map cannot be mutated")}"System.Collections.Generic.ICollection`1.Clear"(){throw new Error("Map cannot be mutated")}"System.Collections.Generic.ICollection`1.Remove2B595"(t){throw new Error("Map cannot be mutated")}"System.Collections.Generic.ICollection`1.Contains2B595"(t){const n=this;return rt(n,t[0])&&F($e(n,t[0]),t[1])}"System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(t,n){Fr(this.tree,t,n)}"System.Collections.Generic.ICollection`1.get_IsReadOnly"(){return!0}"System.Collections.Generic.ICollection`1.get_Count"(){return ke(this)|0}"System.Collections.Generic.IReadOnlyCollection`1.get_Count"(){return ke(this)|0}get size(){return ke(this)|0}clear(){throw new Error("Map cannot be mutated")}delete(t){throw new Error("Map cannot be mutated")}entries(){return he(n=>[n[0],n[1]],this)}get(t){return $e(this,t)}has(t){return rt(this,t)}keys(){return he(n=>n[0],this)}set(t,n){throw new Error("Map cannot be mutated")}values(){return he(n=>n[1],this)}forEach(t,n){const r=this;Sr(o=>{t(o[1],o[0],r)},r)}}function fe(e,t){return new Ce(e,t)}function Dr(e){return fe(e,void 0)}function Rr(e){return e.tree}function jr(e,t,n){return fe(e.comparer,X(e.comparer,t,n,e.tree))}function $e(e,t){return Nr(e.comparer,t,e.tree)}function qr(e,t){Gt(t,e.tree)}function Ur(e,t){return fe(e.comparer,Or(t,e.tree))}function ke(e){return kr(e.tree)}function rt(e,t){return vr(e.comparer,t,e.tree)}function Kr(e,t){return Bt(e.comparer,t,e.tree)}function zr(e){const t=(o,s)=>(o<<1)+s+631;let n=0;const r=k(e);try{for(;r["System.Collections.IEnumerator.MoveNext"]();){const o=r["System.Collections.Generic.IEnumerator`1.get_Current"]();n=t(n,V(o[0]))|0,n=t(n,V(o[1]))|0}}finally{O(r)}return n|0}function Se(e,t,n){return jr(n,e,t)}function Zr(e,t){return $e(t,e)}function xr(e,t){return Kr(t,e)}function Jr(e,t){qr(t,e)}function Vt(e,t){return Ur(t,e)}function Dt(e,t,n){return Lr(e,t,Rr(n))}function ot(e,t){return fe(t,Ht(t,e))}function Xr(e,t){return fe(t,Ht(t,e))}function Rt(e){return Dr(e)}function Yr(e,t,n,r){const o=D(e,n)|0;if(o===0)throw new Error("The step of a range cannot be zero");const s=o>0;return u=>{const i=D(u,t)|0;return s&&i<=0||!s&&i>=0?[u,r(u,e)]:void 0}}function Qr(e,t,n,r,o){const s=Yr(t,n,r,o);return yr(()=>_r(s,e))}function eo(e,t,n){return Qr(e,t,n,0,(r,o)=>r+o)}class Ze extends K{constructor(t,n,r,o){super(),this.Id=t,this.SvgElement=n,this.IsHidden=r,this.IsVisibleLock=o}}function me(e,t){let n;return new Ze(t.getAttribute("inkscape:label"),t,(n=t.getAttribute("visibility"),n===Z()?!1:n==="hidden"),e)}function to(e){return e.IsVisibleLock||e.IsHidden?e:(e.SvgElement.setAttribute("visibility","hidden"),new Ze(e.Id,e.SvgElement,!0,e.IsVisibleLock))}function Ne(e){return e.IsVisibleLock?e:e.IsHidden?(e.SvgElement.setAttribute("visibility",""),new Ze(e.Id,e.SvgElement,!1,e.IsVisibleLock)):e}const no=dt(mt("NuPogodiElectronics.SegmentDisplay",gt,[["None",0],["Top",1],["LeftTop",2],["RightTop",4],["Middle",8],["LeftBottom",16],["RightBottom",32],["Bottom",64]])),jt=no.filter(e=>e!==0);function ro(e){switch(e){case 0:return 119;case 1:return 36;case 2:return 93;case 3:return 109;case 4:return 46;case 5:return 107;case 6:return 123;case 7:return 37;case 8:return 127;case 9:return 111;default:return 0}}function qt(e){switch(e){case 1:return"top";case 2:return"leftTop";case 4:return"rightTop";case 8:return"middle";case 16:return"leftBottom";case 32:return"rightBottom";case 64:return"bottom";default:return I(A("not found asset name for %A"))(e)}}function oo(e){return Yn(t=>{let n;if(n=t|0,(e&n)===n)return qt(t)},jt)}class L extends je{constructor(t,n){super(),this.tag=t,this.fields=n}cases(){return["Group","Node"]}}function so(e,t){const n=r=>{r.tag===1?e(r.fields[0]):Jr((o,s)=>{n(s)},r.fields[0])};n(t)}function de(e,t){const n=r=>r.tag===1?new L(1,[e(r.fields[0])]):new L(0,[Vt((o,s)=>n(s),r.fields[0])]);return n(t)}function st(e,t,n){if(n.tag===0){const r=n.fields[0];return new L(0,[Se(e,t(Zr(e,r)),r)])}else return I(A('expected group in "%s" asset id but `%A`'))(e)(n)}const Ut="rightTopEggs",Kt="rightBottomEggs",zt="leftTopEggs",Zt="leftBottomEggs",xt="leftWolf",Jt="rightWolf",Xt="leftWolfTopHand",Yt="leftWolfBottomHand",Qt="rightWolfTopHand",en="rightWolfBottomHand",tn="rightBrokenEgg",nn="leftBrokenEgg",rn="digit1",on="digit2",sn="digit3",un="digit4",ln="leftTopButton",cn="rightTopButton",an="leftBottomButton",fn="rightBottomButton";class xe extends K{constructor(t,n){super(),this.Container=t,this.Root=n}}function uo(e){const t=(u,i)=>$n(Hn(i.querySelector(`[*|label="${u}"]`)),()=>I(A("not found %s"))(u)),n=u=>{const i=t(u,e);return[u,new L(0,[ot(sr(c=>{const l=De(c);return[l,new L(1,[me(!1,t(l,i))])]},br(eo(1,1,5))),{Compare:te})])]},r=u=>[u,new L(1,[me(!1,t(u,e))])],o=u=>[u,new L(1,[me(!0,t(u,e))])],s=u=>{const i=t(u,e);return[u,new L(0,[Xr(St(c=>{const l=qt(c);return[l,new L(1,[me(!1,t(l,i))])]},jt),{Compare:te})])]};return new xe(ot(Ke([n(Ut),n(Kt),n(zt),n(Zt),r(xt),r(Jt),r(Xt),r(Yt),r(Qt),r(en),r(nn),r(tn),s(rn),s(on),s(sn),s(un),o(ln),o(cn),o(an),o(fn)]),{Compare:te}),e)}function io(e){return new xe(Vt((t,n)=>de(to,n),e.Container),e.Root)}function hn(e,t){return xr(e,t.Container)}function lo(e,t,n){return new xe(Se(e,t,n.Container),n.Root)}function ve(e,t,n){const r=hn(e,n);return r!=null?lo(e,t(r),n):I(A("expected `Some _` but `%A`"))(r)}function co(e,t,n){const r=hn(e,n);return r!=null?t(r):I(A("expected `Some _` but `%A`"))(r)}let ue=Me(!1),ie=Me(!1),le=Me(!1),ce=Me(!1);document.onkeydown=e=>{switch(e.code){case"KeyD":{ue(!0);break}case"KeyA":{ie(!0);break}case"KeyW":{le(!0);break}case"KeyS":{ce(!0);break}}};document.onkeyup=e=>{switch(e.code){case"KeyD":{ue(!1);break}case"KeyA":{ie(!1);break}case"KeyW":{le(!1);break}case"KeyS":{ce(!1);break}}};function ao(e){const t=(n,r)=>{co(n,o=>{so(s=>{const u=s.SvgElement;u.onmousedown=i=>{r(!0)},u.onmouseup=i=>{r(!1)},u.onmouseout=i=>{r(!1)},u.ontouchstart=i=>{r(!0)},u.ontouchend=i=>{r(!1)}},o)},e)};t(ln,n=>{ie(n),le(n)}),t(an,n=>{ie(n),ce(n)}),t(cn,n=>{ue(n),le(n)}),t(fn,n=>{ue(n),ce(n)})}function fo(){let e="";for(let t=0;t++<36;)e+=t*51&52?(t^15?8^Math.random()*(t^20?16:4):4).toString(16):"-";return e}function ho(){return fo()}const mo=dt(mt("NuPogodiElectronics.EggGutter",gt,[["LeftTop",0],["LeftBottom",1],["RightTop",2],["RightBottom",3]]));class mn extends K{constructor(t,n,r){super(),this.Id=t,this.Pos=n|0,this.Gutter=r|0}}const go=5;function po(e){return new mn(ho(),1,e)}class ee extends K{constructor(t,n){super(),this.BodyPos=t|0,this.HandPos=n|0}}function yo(){return new ee(0,0)}class G extends K{constructor(t,n,r,o,s,u){super(),this.Eggs=t,this.Wolf=n,this.Cooldown=r,this.TimeAcc=o,this.BrokenEggPos=s,this.CatchedEggsCount=u|0}}function _o(){return new G(Rt({Compare:te}),yo(),1e3,0,void 0,0)}function bo(){return Math.random()}function Oe(e,t){if(t<e)throw new Error("minValue must be less than maxValue");return Math.floor(Math.random()*(t-e))+e}function wo(e){if(e==null)throw new Error("Buffer cannot be null");for(let t=0;t<e.length;t+=6){let n=Math.floor(Math.random()*281474976710656);const r=Math.floor(n/16777216);for(let o=0;o<6&&t+o<e.length;o++)o===3&&(n=r),e[t+o]=n&255,n>>>=8}}class Eo{constructor(){}Next0(){return Oe(0,2147483647)}Next1(t){return Oe(0,t)}Next2(t,n){return Oe(t,n)}NextDouble(){return bo()}NextBytes(t){wo(t)}}function Co(){return new Eo}function So(){return Co()}function Mo(e){const t=e.Wolf,n=ie()?new ee(0,t.HandPos):t,r=ue()?new ee(1,n.HandPos):n,o=le()?new ee(r.BodyPos,0):r;return new G(e.Eggs,ce()?new ee(o.BodyPos,1):o,e.Cooldown,e.TimeAcc,e.BrokenEggPos,e.CatchedEggsCount)}const To=So();function Ao(e,t){let n;if(t.TimeAcc>t.Cooldown){const r=Dt((s,u,i)=>{const c=i.Pos+1|0;if(c>go){let l;const m=i.Gutter|0,g=s.Wolf.BodyPos|0,y=s.Wolf.HandPos|0;let h;switch(m){case 0:{g===0&&y===0?h=0:h=1;break}case 1:{g===0&&y===1?h=2:h=3;break}case 2:{g===1&&y===0?h=4:h=5;break}case 3:{g===1&&y===1?h=6:h=7;break}default:h=8}switch(h){case 0:{l=void 0;break}case 1:{l=0;break}case 2:{l=void 0;break}case 3:{l=0;break}case 4:{l=void 0;break}case 5:{l=1;break}case 6:{l=void 0;break}case 7:{l=1;break}default:{const d=[m,g,y];l=I(A("not found EggGutter.%A"))([d[0],d[1],d[2]])}}return new G(s.Eggs,s.Wolf,s.Cooldown,s.TimeAcc,l,l!=null?s.CatchedEggsCount:s.CatchedEggsCount+1)}else return new G(Se(u,new mn(i.Id,c,i.Gutter),s.Eggs),s.Wolf,s.Cooldown,s.TimeAcc,s.BrokenEggPos,s.CatchedEggsCount)},new G(Rt({Compare:te}),t.Wolf,t.Cooldown,t.TimeAcc,void 0,t.CatchedEggsCount),t.Eggs),o=new G((n=po(mo[To.Next2(0,4)]),Se(n.Id,n,r.Eggs)),r.Wolf,r.Cooldown,r.TimeAcc,r.BrokenEggPos,r.CatchedEggsCount);return new G(o.Eggs,o.Wolf,o.Cooldown,0,o.BrokenEggPos,o.CatchedEggsCount)}else return new G(t.Eggs,t.Wolf,t.Cooldown,e+t.TimeAcc,t.BrokenEggPos,t.CatchedEggsCount)}function Io(e,t){let n,r,o,s,u,i,c;const l=(p,w)=>ve(p,f=>de(Ne,f),w),m=Dt((p,w,f)=>{let E,C;return ve((E=f.Gutter|0,E===0?zt:E===1?Zt:E===2?Ut:E===3?Kt:I(A("not found `EggGutter.%A`"))(E)),(C=De(f.Pos),z=>st(C,P=>de(Ne,P),z)),p)},io(e),t.Eggs);let g;const y=t.BrokenEggPos;if(y==null)g=m;else{const p=y|0;g=l(p===0?nn:p===1?tn:I(A("expected `BrokenEggPos.Left` or `BrokenEggPos.Right` but `%A`"))(p),m)}const h=l((n=t.Wolf.BodyPos|0,n===0?xt:n===1?Jt:I(A("expected `WolfPos.Left` or `WolfPos.Right` but `%A`"))(n)),g),d=l((r=t.Wolf,o=r.BodyPos|0,s=r.HandPos|0,u=[o,s],o===0?s===0?Xt:s===1?Yt:(i=u,I(A("expected `WolfPos.Left` or `WolfPos.Right` but `%A`"))([i[0],i[1]])):o===1?s===0?Qt:s===1?en:(i=u,I(A("expected `WolfPos.Left` or `WolfPos.Right` but `%A`"))([i[0],i[1]])):(i=u,I(A("expected `WolfPos.Left` or `WolfPos.Right` but `%A`"))([i[0],i[1]]))),h);return[(c=(p,w,f)=>ve(p,E=>Qn((C,z)=>st(z,P=>de(Ne,P),C),E,oo(ro(~~(t.CatchedEggsCount/Math.pow(10,w-1))%10))),f),c(un,4,c(sn,3,c(on,2,c(rn,1,d))))),t]}function ko(e,t,n){return Io(t,Ao(e,Mo(n)))}function No(e){const t=n=>{window.requestAnimationFrame(r=>{const o=ko(r-n[0],n[1],n[2]);t([r,o[0],o[1]])})};ao(e),t([0,e,_o()])}const ut=dn,j=document.createElement("div"),gn=document.querySelector("#app");gn.setAttribute("style","height: 100%;");gn.appendChild(j);j.innerText="Loading...";j.setAttribute("style","height: 100%;");function vo(){let e,t;return t=Ir(ut,At()).then(r=>r.text()),e=t.then(r=>{const u=new DOMParser().parseFromString(r,"image/svg+xml").documentElement;return u.setAttribute("width","100%"),u.setAttribute("height","100%"),new Ye(0,[u])}),e.catch(r=>{let o;return new Ye(1,[(o=r.message,Un(A(`error load %s
%s`))(ut)(o))])})}vo().then(t=>{if(t.tag===1)j.innerText=t.fields[0];else{const n=t.fields[0];j.removeChild(j.firstChild),j.appendChild(n),No(uo(n))}});
