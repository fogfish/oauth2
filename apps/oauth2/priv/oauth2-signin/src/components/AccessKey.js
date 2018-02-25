import React from 'react'
import { Label, Input } from 'react-dress-code'

const AccessKey = () => (
   <div>
      <Label sub="required">Email</Label>
      <Input 
         id="access" 
         type="email"
         autoFocus
         required />
   </div>
)

export default AccessKey
