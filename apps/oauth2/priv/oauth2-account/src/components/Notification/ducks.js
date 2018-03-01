
//
//
const ERROR = "@@notification/error"
const RESET = "@@notification/reset"

//
export const fail = (reason) => {
  switch (reason.type) {
    case 'https://httpstatuses.com/401':
      return {type: ERROR, reason: 'Unauthorized', description: 'Access is Expired. Refresh page to sign-up again.' }

    case 'https://httpstatuses.com/500':
      switch (reason.details) {
        case 'invalid_uri':
          return {type: ERROR, reason: 'Bad Arguments', description: 'Invalid redirect URI. It must define schema, authority and path (http://www.example.com/redirect.html.' }
        default:
          return {type: ERROR, reason: 'Unknown', description: 'Unable to execute operation. Try again later.' }
      }

    default:
      return {type: ERROR, reason: 'Unknown', description: 'Unable to execute operation. Try again later.' }
  }
}

//
export const reset = () => 
  (dispatch, getState) => {
    dispatch({type: RESET})
  }

//
//
const empty = {
  error: undefined
}

export default (state = empty, action) => {
  switch (action.type) {
    case ERROR:
      const error = { reason: action.reason, description: action.description }
      return { ...state, error }
    case RESET:
      return { ...empty }
    default:
      return { ...state }
  }
}
