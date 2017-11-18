
//
// default empty status
const empty = {
   isOAuthApps: true,
   isOAuthAppRegister: false,
   isOAuthAppCredentials: false,

   access_token: window.localStorage.getItem('access_token')
}

//
//
const APPSVIEW = "@@core/appsview";
const REGISTER = "@@core/register";
const CREDENTIALS = "@@core/credentials";


//
//
export default (state = empty, action) => {
   switch(action.type)
   {
   case APPSVIEW:
      return {...state, isOAuthApps: true, isOAuthAppRegister: false, isOAuthAppCredentials: false};

   case REGISTER:
      return {...state, isOAuthApps: false, isOAuthAppRegister: true, isOAuthAppCredentials: false};

   case CREDENTIALS:
      return {...state, isOAuthApps: false, isOAuthAppRegister: false, isOAuthAppCredentials: true};

   default:
      return state
   }
}


export const appsview = () => ({type: APPSVIEW})

export const register = () => ({type: REGISTER})

export const credentials = () => ({type: CREDENTIALS})
