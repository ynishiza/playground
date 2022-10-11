export declare const x: string;

export declare type SomeConst = 2 | 1;

declare function myFn(): void;

export declare class MyClass {
}

export namespace MyClass {
  function doSomething(): void;
}

declare type MyType = {};

declare interface IInterf {
  x: string;
  y(): void;
}
export declare namespace MyClass {
  export const z: string;
}
declare namespace IInterf {
  export const z: string;
}

declare namespace myFn {
  const z: string;
}
declare namespace myFn.abc {
  const d: string;
}
