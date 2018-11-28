import React from 'react'
import { Dialog, DialogContent, DialogBody, DialogTitle, DialogSubTitle, DialogActions, Button } from 'react-dress-code'
import { Link } from 'react-router-dom'
import { KeyPair } from './KeyPair'

const SignUp = props => (
  <Dialog>
    <DialogContent>
      <form className="form-group" action="/oauth2/signup" method="post">
        <DialogBody>
          <DialogTitle>
            <i className="fa fa-user-circle-o" aria-hidden="true"></i>
            &nbsp; &nbsp; Sign Up
          </DialogTitle>
          <KeyPair { ...props } />
        </DialogBody>
        <DialogActions with-link>
          <Link className="dc-link" to="/"><b>Sign In instead</b></Link>
          <Button primary type="submit">Sign Up</Button>
        </DialogActions>
      </form>
    </DialogContent>
  </Dialog>
)

export default SignUp
