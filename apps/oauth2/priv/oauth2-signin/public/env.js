// This file will not end up inside the main application JavaScript bundle.
// Instead, it will simply be copied inside the build folder.
// The generated "index.html" will require it just before this main bundle.
// You can thus use it to define some environment variables that will
// be made available synchronously in all your JS modules under "src". 
//
// set feature value to false to disable it
window.env = {
   //
   // Enables SignIn/SignUp using access/secret key pair
   KEYPAIR: true,

   //
   // Enable SignIn using GitHub account
   GITHUB: "https://github.com/login/oauth/authorize?client_id=xxx&scope=xxx"
}
