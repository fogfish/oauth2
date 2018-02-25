import queryString from 'query-string'

const response_type = (json) => (
  json.response_type ? json.response_type : "code"
)

const client_id = (json) => (
  json.client_id ? json.client_id : "oauth2-account"
)

const state = (json) => (
  json.state ? json.state : ""
)

const defaults = (json) => (
  {
    response_type: response_type(json),
    client_id: client_id(json),
    state: state(json)
  }
)

//
// default state
const empty = {
   authRequest: defaults(queryString.parse(window.location.search))
}

//
//
export default (state = empty, action) => {
   switch(action.type)
   {
   default:
      return state
   }
}
