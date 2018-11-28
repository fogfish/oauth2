import React from 'react'
import { Label, Input } from 'react-dress-code'

export const SecretKey = () => (
  <div className="dc-input-stack">
    <Label sub="required">Password</Label>
    <Input 
      id="secret" 
      type="password"
      required
    />
  </div>
)
