import queryString from 'query-string'

const defaults = (json) => (
   {
      response_type: json.response_type ? json.response_type : "code",
      client_id: json.client_id ? json.client_id : "oauth2-account",
      state: json.state ? json.state : ""
   }
)

//
// default state
const empty = {
   isSignIn: true,
   isSignUp: false,

   authRequest: defaults(queryString.parse(window.location.search))
}


//
//
export const SIGNUP = "@@core/signup" 
export const SIGNIN = "@@core/signin" 


//
//
export default (state = empty, action) => {
   switch(action.type)
   {
   case SIGNUP:
      return {...state, isSignUp: true, isSignIn: false}
   case SIGNIN:
      return {...state, isSignUp: false, isSignIn: true}
   default:
      return state
   }
}

export const signin = () => ({type: SIGNIN})
export const signup = () => ({type: SIGNUP})
