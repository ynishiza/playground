import myModule from './myModule';
import _ from 'lodash';
import moduleWithType from '../jsModuleWithTypes';
import '../jsExtendGlobal/index';
// import { main as m } from 'jsModuleWithoutTypes';
// m();
import moduleWithoutTypess from '../jsModuleWithoutTypes';
// jsExtendGlobal
// console.log(myglobalvalue);

async function main() {
  myModule('hello');

  console.log(`1: ${_.isPlainObject(_)}`);

  let a : string = 'abcd';
  console.log(a);

  await runTest(testJsModules);
  await runTest(testTypes);
  await runTest(testObject);
  await runTest(testVoid);
  await runTest(testFunctionOverload);
  await runTest(testGenerics);
  await runTest(testInterfaces);
  await runTest(testSignatures);
  await runTest(testKeyOf);
  await runTest(_testThis);
  await runTest(_testOverload);
}

async function runTest(fn: Function) {
  console.log(`====================Start test ${fn.name}====================`);
  await fn();
  console.log(`====================Done test ${fn.name}====================\n`);
}

async function testTypes() {
  // let x : string = 'a';
  // let x : null = null;
  let x : undefined = undefined;
  console.log(x);

  let y : (a: string | number, b: string) => string | number = (a, b) => a + b;
  let z = (a : string) : string => a;
  console.log(y(1, 'a'));
  console.log(z('b'));

  let literal : 1 | 2 | 3 = 1;
  literal = 3;
  // literal = 4;
  console.log(literal);
}

async function testJsModules() {
  moduleWithType('a');
  moduleWithoutTypess();
}

async function testInterfaces() {
  interface MyArray {
    [index: number]: any;
    length: number;
  };
  interface MyMap {
    [index: string]: any;
  };

  let x : MyArray = [1,2,3];
  let y : MyMap = {a:1 };
  console.log(x);
  console.log(y);
}

async function testObject() {
  interface MyTypeConstructor {
    new (): MyType;
  }
  interface MyType {
    toString(): string;
  }
  class ConstructorTestClass implements MyType {
    constructor() { }
    toString() { return "Hello from ConstructorTestClass"; }
  }
  let x : MyTypeConstructor = ConstructorTestClass;
  let y : MyType = new x();
  console.log(y.toString());
}

async function testVoid() {
  let x : () => void;
  x = () => null;
  x = () => {};
  x = () => true;
  x = () => undefined;
  console.log(x());

  let y : () => undefined;
  y = () => undefined;
  console.log(y());

  function a(): void {
    // return 1;
    return undefined;
    // return;
  }
  console.log(a());
}

async function testSignatures() {
  function myfn({ x }: { x:string } = { x: 'default'  }) {
    console.log(x);
  }
  function myfn11({ x = 'default' }: { x?: string }) {
    console.log(x);
  }

  myfn({ x: 'hello'});
  // myfn({});
  myfn();
  myfn11({});
  myfn11({ x: 'withdef' });
  myfn11({ x: undefined });

  let myFn2: (x: any) => Promise<string> = async(x: any) => x;
  console.log(await myFn2(1));
  console.log(await myFn2(10));

  async function myFn3(x: any): Promise<any> {
    return x + 1;
  }
  console.log(await myFn3(1));
  myFn2 = myFn3;
  console.log(await myFn2(5));

  const { y = 'a' }: { y?: string } = {};
  const { z }: { z: string } = { z: 'b' };
  console.log(y);
  console.log(z);
}

async function testKeyOf() {
  type MyOptions<T> = {
    [prop in keyof T]: boolean;
  }

  const a = {
    x: 1,
    y: 2,
  };
  const aOpts: MyOptions<typeof a> = {
    x: true,
    y: false,
  };
  console.log(a, aOpts);

  interface IObj {
    [k: string]: any;
  }
  const x: IObj = { a: 1 };
  // const x: Object = { a: 1 };
  x.a = 1;
  function wrap<T extends IObj>(obj: T): MyOptions<T> {
    return Object.keys(obj).reduce((aggr, k) => ({
      [k]: !!obj[k],
      ...aggr,
    }), {}) as MyOptions<T>;
    // const res: any = {};
    // Object.keys(obj).forEach((k: string) => {
    //   res[k] = !!obj[k];
    // });
    // return res;
    // return _.mapValues(obj, v => !!v);
  }
  function wrap2<T, K extends keyof T>(obj: T): MyOptions<T> {
    return Object.keys(obj).reduce((aggr, k) => ({
      // [k]: !!obj[k as K],
      [k]: !!obj[k as K],
      ...aggr,
    }), {}) as MyOptions<T>;
    // const res: any = {};
    // Object.keys(obj).forEach((k: string) => {
    //   res[k] = !!obj[k];
    // });
    // return res;
    // return _.mapValues(obj, v => !!v);
  }
  console.log(wrap({ a: 1, b: 0 }));
  console.log(wrap2({ a: 1, b: 0 }));
}

async function testFunctionOverload() {
  function myFn(): void;
  function myFn(x: number): number;
  function myFn(x: number, y: number): number;
  function myFn(x?: number, y?: number): number | void {
    if (x == null) return;

    return typeof y === 'number' ? x + y : x;
  }

  console.log(myFn(1));
  console.log(myFn(1, 2));
  console.log(myFn());
}

async function testGenerics() {
  let x : <T>(a: T) => T;
  x = a => a;
  console.log(x(1));
  console.log(x('s'));

  function myFn<T>(a: T): T {
    return a;
  }
  console.log(myFn(1));
  console.log(myFn('a'));

  type MyType<T> = {
    value: T;
  }
  class MyClass<T> implements MyType<T> {
    constructor(value: T) {
      this.value = value;
    }
    value: T;
  }
  let a = new MyClass(100);
  let b = new MyClass('abc');
  console.log(a.value);
  console.log(b.value);

  type MyVal = MyType<string> extends MyType<string> ? string : boolean;
  type MyKey = keyof Array<any>;
  let c : MyVal = 'a';
  let d : MyKey = 1;
  console.log(c);
  console.log(d);


  type MyValue = number | string;
  type MyConstrainedType<T extends MyValue> = {
    value: T;
  }
  let ab : MyConstrainedType<string> = { value: 'a' };
  console.log({ value: 1 } as MyConstrainedType<number>);
  // console.log({ value: true } as MyConstrainedType<boolean>);
  console.log(ab);
}

function _testThis() {
  const a = {
    value: 1,
  };
  function fn(this: typeof a, x: number) {
    console.log(this.value + x);
  }
  fn.call(a, 2);
}

function _testOverload() {
  function myfn(x: number): number;
  function myfn(x: number, y: number): number;
  function myfn(x: number, y?: number) {
    return (y != null ? y : 1) * x;
  }

  type Call = (info: any) => any;
  function myfn2(call: Call): void;
  function myfn2(opts: { info: any }, call: Call): void;
  function myfn2(arg1: Call | { info: any }, arg2?: Call) {
    let call;
    let info = {};
    if (!_.isFunction(arg1)) {
      info = arg1.info;
      call = arg2;
    } else {
      call = arg1;
    }
    if (!call) throw new Error('oops');

    console.log(`result=${call(info)}`);
  }

  console.log(myfn(3));
  console.log(myfn(10, 3));

  myfn2(() => 2);
  myfn2({ info: { x: 1 } }, (info: any) => 2 + info.x);
}

(main().catch(e => console.log(e)));
