import React from 'react'
import { Label, Input } from 'react-dress-code'

const SecretKey = () => (
   <div>
      <Label sub="required">Password</Label>
      <Input 
         id="secret" 
         type="password"
         required />
   </div>
)

export default SecretKey
