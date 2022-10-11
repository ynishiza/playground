declare module "jsModuleWithoutTypes" {
  function main(y?: any): void;
  // export namespace main {
  //   const x: string;
  // }
  export = main
}

// export = jsModuleWithoutTypes;
// export as namespace jsModuleWithoutTypes;
// declare function jsModuleWithoutTypes(): void;
// declare namespace jsModuleWithoutTypes {
// }
// Why doesn't this work?
// export = main;
// declare function main(msg: any): void;
