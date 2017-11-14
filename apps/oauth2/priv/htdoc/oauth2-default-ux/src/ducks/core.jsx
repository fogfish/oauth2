
//
// default state
const empty = {
   isSignIn: true,
   isSignUp: false
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
